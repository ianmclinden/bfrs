// The language consists of eight commands. A brainfuck program is a sequence of these commands, possibly interspersed with other characters (which are ignored). The commands are executed sequentially, with some exceptions: an instruction pointer begins at the first command, and each command it points to is executed, after which it normally moves forward to the next command. The program terminates when the instruction pointer moves past the last command.

use std::{
    fmt::Display,
    io::{ErrorKind, Read, Write},
    thread,
    time::Duration,
};

use clap::{crate_name, crate_version};

const TAPE_SIZE: usize = 30_000;
const QUIT_SYMBOLS: &[&str] = &["q", "quit"];

pub type Result<T> = std::result::Result<T, BFMachineError>;

#[derive(Clone, PartialEq)]
pub enum BFMachineError {
    InvalidCommand(u8),
    UnmatchedBracket,
    OutOfBounds,
    ReadError,
    WriteError,
    Quit,
    Unknown,
}

impl std::fmt::Display for BFMachineError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            BFMachineError::InvalidCommand(c) => write!(f, "Invalid command: '{}'", *c as char),
            BFMachineError::UnmatchedBracket => write!(f, "Unmatched brackets"),
            BFMachineError::OutOfBounds => write!(f, "Out of bounds"),
            BFMachineError::ReadError => write!(f, "Read error"),
            BFMachineError::WriteError => write!(f, "Write error"),
            BFMachineError::Quit => write!(f, "Quitting"),
            BFMachineError::Unknown => write!(f, "Unknown error"),
        }
    }
}

impl std::fmt::Debug for BFMachineError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
enum Command {
    PointerRight,
    PointerLeft,
    Increment,
    Decrement,
    OutputByte,
    ReadByte,
    JumpForward(usize),
    JumpBack(usize),
    Ignored(u8),
    End,
}

impl From<u8> for Command {
    fn from(value: u8) -> Self {
        match value {
            b'>' => Command::PointerRight,
            b'<' => Command::PointerLeft,
            b'+' => Command::Increment,
            b'-' => Command::Decrement,
            b'.' => Command::OutputByte,
            b',' => Command::ReadByte,
            b'[' => Command::JumpForward(0),
            b']' => Command::JumpBack(0),
            0 => Command::End,
            i => Command::Ignored(i),
        }
    }
}

impl From<&u8> for Command {
    fn from(value: &u8) -> Self {
        Command::from(*value)
    }
}

impl From<Command> for char {
    fn from(val: Command) -> Self {
        match val {
            Command::PointerRight => '>',
            Command::PointerLeft => '<',
            Command::Increment => '+',
            Command::Decrement => '-',
            Command::OutputByte => '.',
            Command::ReadByte => ',',
            Command::JumpForward(_) => '[',
            Command::JumpBack(_) => ']',
            Command::Ignored(i) => i as char,
            Command::End => 0 as char,
        }
    }
}

impl Display for Command {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", char::from(*self))
    }
}

/// The brainfuck language uses a simple machine model consisting of the program and instruction pointer, as well as a one-dimensional array of at least 30,000 byte cells initialized to zero; a movable data pointer (initialized to point to the leftmost byte of the array); and two streams of bytes for input and output (most often connected to a keyboard and a monitor respectively, and using the ASCII character encoding).
#[derive(Debug, Clone)]
pub struct BfMachine {
    /// A one-dimensional array of at least 30,000 byte cells initialized to zero
    data: Vec<u8>,
    /// A movable data pointer (initialized to point to the leftmost byte of the array)
    data_ptr: usize,
    /// Maximum value ever reached by the data pointer
    max_data_ptr: usize,
    /// A one-dimensional array of instructions
    instructions: Vec<Command>,
    /// A movable instruction pointer (initialized to point to the leftmost byte of the program)
    inst_ptr: usize,
    /// Number of bytes output
    output_bytes: usize,
    /// Delay between execution steps
    step_delay: Option<Duration>,
    /// Maximum number of steps to execute
    max_steps: Option<usize>,
}

impl Default for BfMachine {
    fn default() -> Self {
        Self::new(TAPE_SIZE)
    }
}

impl Drop for BfMachine {
    fn drop(&mut self) {
        self.print_debug();
    }
}

impl BfMachine {
    pub fn new(tape_size: usize) -> Self {
        Self {
            data: vec![0; tape_size],
            data_ptr: 0,
            max_data_ptr: 0,
            instructions: Vec::new(),
            inst_ptr: 0,
            output_bytes: 0,
            step_delay: None,
            max_steps: None,
        }
    }

    pub fn set_delay(&mut self, millis: u64) {
        self.step_delay = Some(Duration::from_millis(millis));
    }

    pub fn reset(&mut self) {
        self.print_debug();
        let size = self.data.capacity();
        self.data = vec![0; size];
        self.data_ptr = 0;
        self.instructions = Vec::new();
        self.inst_ptr = 0;
        self.output_bytes = 0;
    }

    fn print_debug(&self) {
        log::debug!("DC: {}", self.data_ptr);
        log::debug!("IC: {}", self.inst_ptr);
        log::debug!("Tape: {:?}", self.format_tape());
    }

    fn format_tape(&self) -> String {
        let mut d = self.data.clone();
        d.drain(self.max_data_ptr + 1..);
        format!(
            "[{}]",
            d.iter()
                .enumerate()
                .map(|(i, d)| match i {
                    f if f == self.data_ptr => format!("\x1b[;7m{d}\x1b[0m"),
                    _ => format!("{d}"),
                })
                .collect::<Vec<String>>()
                .join(", ")
        )
    }

    fn format_instructions(&self) -> String {
        self.instructions
            .iter()
            .enumerate()
            .map(|(i, c)| match i {
                f if f == self.inst_ptr => format!("\x1b[;7m{c}\x1b[0m"),
                _ => format!("{c}"),
            })
            .collect::<String>()
    }

    /// > Increment the data pointer by one (to point to the next cell to the right).
    fn ptr_right(&mut self) -> Result<()> {
        match self.data_ptr.checked_add(1) {
            Some(p) => {
                self.data_ptr = p;
                self.max_data_ptr = std::cmp::max(self.data_ptr, self.max_data_ptr);
                Ok(())
            }
            None => Err(BFMachineError::OutOfBounds),
        }
    }

    /// < Decrement the data pointer by one (to point to the next cell to the left).
    fn ptr_left(&mut self) -> Result<()> {
        match self.data_ptr.checked_sub(1) {
            Some(p) => {
                self.data_ptr = p;
                Ok(())
            }
            None => Err(BFMachineError::OutOfBounds),
        }
    }

    fn set(&mut self, val: u8) -> Result<()> {
        match self.data.get_mut(self.data_ptr) {
            Some(p) => {
                *p = val;
                Ok(())
            }
            None => Err(BFMachineError::OutOfBounds),
        }
    }

    /// + Increment the byte at the data pointer by one.
    fn increment(&mut self) -> Result<()> {
        match self.data.get_mut(self.data_ptr) {
            Some(p) => {
                *p = p.wrapping_add(1);
                Ok(())
            }
            None => Err(BFMachineError::OutOfBounds),
        }
    }

    /// - Decrement the byte at the data pointer by one.
    fn decrement(&mut self) -> Result<()> {
        match self.data.get_mut(self.data_ptr) {
            Some(p) => {
                *p = p.wrapping_sub(1);
                Ok(())
            }
            None => Err(BFMachineError::OutOfBounds),
        }
    }

    /// . Output the byte at the data pointer.
    fn output_byte<W: Write>(&mut self, writer: &mut W) -> Result<()> {
        match self.data.get(self.data_ptr) {
            Some(c) => match (write!(writer, "{}", *c as char), writer.flush()) {
                (Ok(_), Ok(_)) => {
                    self.output_bytes += 1;
                    Ok(())
                }
                _ => Err(BFMachineError::WriteError),
            },
            None => Err(BFMachineError::OutOfBounds),
        }
    }

    /// , Accept one byte of input, storing its value in the byte at the data pointer.
    fn read_byte<R: Read>(&mut self, reader: &mut R) -> Result<()> {
        let mut buffer = [0u8; 1];
        match reader.read_exact(&mut buffer) {
            Ok(_) => self.set(buffer.first().unwrap().to_owned()),
            Err(e) if e.kind() == ErrorKind::UnexpectedEof => Err(BFMachineError::Quit),
            Err(_) => Err(BFMachineError::ReadError),
        }
    }

    /// [ If the byte at the data pointer is zero, then instead of moving the instruction pointer forward to the next command, jump it forward to the command after the matching ] command.
    /// [ and ] match as parentheses usually do: each [ matches exactly one ] and vice versa, the [ comes first, and there can be no unmatched [ or ] between the two.
    fn jmp_fwd(&mut self, nest: usize) -> Result<()> {
        match self.data.get(self.data_ptr) {
            Some(v) => match v {
                0 => {
                    match self.instructions[self.inst_ptr..]
                        .iter()
                        .enumerate()
                        .find(|(_, &c)| c == Command::JumpBack(nest))
                    {
                        Some((i, _)) => {
                            self.inst_ptr += i;
                            Ok(())
                        }
                        None => Err(BFMachineError::UnmatchedBracket),
                    }
                }
                _ => Ok(()),
            },
            None => Err(BFMachineError::OutOfBounds),
        }
    }

    /// ] If the byte at the data pointer is nonzero, then instead of moving the instruction pointer forward to the next command, jump it back to the command after the matching [ command.
    /// [ and ] match as parentheses usually do: each [ matches exactly one ] and vice versa, the [ comes first, and there can be no unmatched [ or ] between the two.
    fn jmp_back(&mut self, nest: usize) -> Result<()> {
        match self.data.get(self.data_ptr) {
            Some(v) => match v {
                0 => Ok(()),
                _ => {
                    match self.instructions[..=self.inst_ptr]
                        .iter()
                        .rev()
                        .enumerate()
                        .find(|(_, &c)| c == Command::JumpForward(nest))
                    {
                        Some((i, _)) => {
                            self.inst_ptr -= i;
                            Ok(())
                        }
                        None => Err(BFMachineError::UnmatchedBracket),
                    }
                }
            },
            None => Err(BFMachineError::OutOfBounds),
        }
    }

    fn load_program(&mut self, program: &str) -> Result<()> {
        // Read through and parse the bracket depth
        let mut bracket_depth: usize = 0;
        for i in program
            .split_whitespace()
            .collect::<String>()
            .as_bytes()
            .iter()
        {
            let cmd = match Command::from(i) {
                Command::JumpForward(_) => {
                    let c = Command::JumpForward(bracket_depth);
                    bracket_depth += 1;
                    Ok(c)
                }
                Command::JumpBack(_) => match bracket_depth.checked_sub(1) {
                    Some(c) => {
                        bracket_depth = c;
                        Ok(Command::JumpBack(bracket_depth))
                    }
                    None => Err(BFMachineError::UnmatchedBracket),
                },
                c => Ok(c),
            }?;
            self.instructions.push(cmd);
        }

        if bracket_depth != 0 {
            return Err(BFMachineError::UnmatchedBracket);
        }
        Ok(())
    }

    fn run<R: Read, W: Write>(
        &mut self,
        reader: &mut R,
        writer: &mut W,
        program: &str,
    ) -> Result<()> {
        self.load_program(program)?;

        let mut steps: usize = 0;

        while let Some(command) = self.instructions.get(self.inst_ptr) {
            log::debug!("{}", self.format_tape());
            log::debug!("{}", self.format_instructions());

            let res = match command {
                Command::PointerRight => self.ptr_right(),
                Command::PointerLeft => self.ptr_left(),
                Command::Increment => self.increment(),
                Command::Decrement => self.decrement(),
                Command::OutputByte => self.output_byte(writer),
                Command::ReadByte => self.read_byte(reader),
                Command::JumpForward(i) => self.jmp_fwd(*i),
                Command::JumpBack(i) => self.jmp_back(*i),
                Command::Ignored(i) => Err(BFMachineError::InvalidCommand(*i)),
                Command::End => Err(BFMachineError::Quit),
            };
            // Just a normal quit, not an error
            if let Err(BFMachineError::Quit) = res {
                return Ok(());
            }

            if let Some(max) = self.max_steps {
                match steps.checked_add(1) {
                    Some(s) if s > max => return Ok(()),
                    Some(s) => steps = s,
                    None => return Err(BFMachineError::OutOfBounds),
                }
            }

            self.inst_ptr += 1;

            if let Some(delay) = self.step_delay {
                thread::sleep(delay);
            }
        }
        Ok(())
    }

    pub fn execute(&mut self, program: &str) -> Result<()> {
        self.run(&mut std::io::stdin(), &mut std::io::stdout(), program)
    }

    pub fn interactive(&mut self) -> Result<()> {
        println!("{} {}", crate_name!(), crate_version!());
        println!(
            "Ctrl+C, or type {} to quit.",
            QUIT_SYMBOLS
                .iter()
                .map(|s| format!("'{s}'"))
                .collect::<Vec<String>>()
                .join(", ")
        );

        let stdin = std::io::stdin();

        loop {
            match self.output_bytes {
                0 => print!(">>> "),
                _ => print!("\n>>> "),
            };
            std::io::stdout()
                .flush()
                .map_err(|_| BFMachineError::WriteError)?;

            self.reset();

            let mut program = String::new();
            stdin
                .read_line(&mut program)
                .map_err(|_| BFMachineError::ReadError)?;
            let program = program.trim();

            if QUIT_SYMBOLS.contains(&program) {
                return Ok(());
            }

            if let Err(e) = self.execute(program) {
                eprintln!("Error: {e}");
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::{BFMachineError, BfMachine, Command};

    #[test]
    fn test_parse_command() {
        let test_values = Vec::from([
            (b'>', Command::PointerRight),
            (b'<', Command::PointerLeft),
            (b'+', Command::Increment),
            (b'-', Command::Decrement),
            (b'.', Command::OutputByte),
            (b',', Command::ReadByte),
            (b'[', Command::JumpForward(0)),
            (b']', Command::JumpBack(0)),
            (0, Command::End),
            (255, Command::Ignored(255)),
        ]);
        for (input, expected) in test_values.into_iter() {
            let c = Command::from(input);
            assert_eq!(c, expected);
        }
    }

    #[test]
    fn test_output_command() {
        let test_values = Vec::from([
            (Command::PointerRight, '>'),
            (Command::PointerLeft, '<'),
            (Command::Increment, '+'),
            (Command::Decrement, '-'),
            (Command::OutputByte, '.'),
            (Command::ReadByte, ','),
            (Command::JumpForward(0), '['),
            (Command::JumpBack(0), ']'),
            (Command::End, 0 as char),
            (Command::Ignored(255), 255 as char),
        ]);
        for (input, expected) in test_values.into_iter() {
            let c: char = input.into();
            assert_eq!(c, expected);
        }
    }

    #[test]
    fn test_pointer_right() {
        let mut bf = BfMachine::default();
        assert_eq!(bf.data_ptr, 0);
        assert_eq!(bf.inst_ptr, 0);

        assert!(bf.ptr_right().is_ok());
        assert_eq!(bf.data_ptr, 1);
        assert_eq!(bf.inst_ptr, 0);

        bf.data_ptr = usize::MAX;
        assert!(bf.ptr_right().is_err());
    }

    #[test]
    fn test_pointer_left() {
        let mut bf = BfMachine::default();
        assert_eq!(bf.data_ptr, 0);
        assert_eq!(bf.inst_ptr, 0);

        assert!(bf.ptr_left().is_err());
        bf.data_ptr = 1;

        assert!(bf.ptr_left().is_ok());
        assert_eq!(bf.data_ptr, 0);
        assert_eq!(bf.inst_ptr, 0);
    }

    #[test]
    fn test_set() {
        let mut bf = BfMachine::default();
        let mut d0 = bf.data.get(bf.data_ptr).unwrap();
        assert_eq!(d0, &0);

        assert!(bf.set(127).is_ok());
        d0 = bf.data.get(bf.data_ptr).unwrap();
        assert_eq!(d0, &127);
    }

    #[test]
    fn test_pointer_increment() {
        let mut bf = BfMachine::default();
        let mut d0 = bf.data.get(bf.data_ptr).unwrap();
        assert_eq!(d0, &0);

        assert!(bf.increment().is_ok());
        d0 = bf.data.get(bf.data_ptr).unwrap();
        assert_eq!(d0, &1);

        let d0 = bf.data.get_mut(bf.data_ptr).unwrap();
        *d0 = u8::MAX;

        assert!(bf.increment().is_ok());
        let d0 = bf.data.get(bf.data_ptr).unwrap();
        assert_eq!(d0, &0);
    }

    #[test]
    fn test_pointer_decrement() {
        let mut bf = BfMachine::default();
        let mut d0 = bf.data.get(bf.data_ptr).unwrap();
        assert_eq!(d0, &0);

        assert!(bf.decrement().is_ok());
        d0 = bf.data.get(bf.data_ptr).unwrap();
        assert_eq!(d0, &u8::MAX);

        assert!(bf.decrement().is_ok());
        let d0 = bf.data.get(bf.data_ptr).unwrap();
        assert_eq!(d0, &(u8::MAX - 1));
    }

    #[test]
    fn test_pointer_output_byte() {
        let mut bf = BfMachine::default();
        assert_eq!(bf.output_bytes, 0);
        let mut output: Vec<u8> = Vec::new();
        assert!(bf.set(b'&').is_ok());

        assert!(bf.output_byte(&mut output).is_ok());
        let d0 = bf.data.get(bf.data_ptr).unwrap();
        let e0 = output.first().unwrap();
        assert_eq!(d0, e0);
        assert_eq!(bf.output_bytes, 1);
    }

    #[test]
    fn test_pointer_read_byte() {
        let mut bf = BfMachine::default();
        let mut input = "ab".as_bytes();

        let d0 = bf.data.get(bf.data_ptr).unwrap();
        assert_eq!(d0, &0);

        assert!(bf.read_byte(&mut input).is_ok());
        let d0 = bf.data.get(bf.data_ptr).unwrap();
        assert_eq!(d0, &b'a');

        assert!(bf.read_byte(&mut input).is_ok());
        let d0 = bf.data.get(bf.data_ptr).unwrap();
        assert_eq!(d0, &b'b');

        let read = bf.read_byte(&mut input);
        assert_eq!(read, Err(BFMachineError::Quit));
        let d0 = bf.data.get(bf.data_ptr).unwrap();
        assert_eq!(d0, &b'b');
    }

    #[test]
    fn test_pointer_jmp_fwd() {
        let mut bf = BfMachine::default();
        let program: &str = "[this_is_a_comment]"; // easy, no whitespace
        assert!(bf.load_program(program).is_ok());
        assert!(bf.set(1).is_ok());
        assert_eq!(bf.data_ptr, 0);
        assert_eq!(bf.inst_ptr, 0);

        assert!(bf.jmp_fwd(99).is_ok());
        assert_eq!(bf.data_ptr, 0);
        assert_eq!(bf.inst_ptr, 0);

        assert!(bf.set(0).is_ok());
        assert_eq!(bf.jmp_fwd(99), Err(BFMachineError::UnmatchedBracket));
        assert_eq!(bf.data_ptr, 0);
        assert_eq!(bf.inst_ptr, 0);

        assert!(bf.jmp_fwd(0).is_ok());
        assert_eq!(bf.data_ptr, 0);
        assert_eq!(bf.inst_ptr, program.len() - 1);
    }

    #[test]
    fn test_pointer_jmp_back() {
        let mut bf = BfMachine::default();
        let program: &str = "[this_is_a_comment]"; // easy, no whitespace
        assert!(bf.load_program(program).is_ok());
        bf.inst_ptr = program.len() - 1;

        assert!(bf.jmp_back(0).is_ok());
        assert_eq!(bf.data_ptr, 0);
        assert_eq!(bf.inst_ptr, program.len() - 1);

        assert!(bf.set(1).is_ok());
        assert_eq!(bf.jmp_back(99), Err(BFMachineError::UnmatchedBracket));
        assert_eq!(bf.data_ptr, 0);
        assert_eq!(bf.inst_ptr, program.len() - 1);

        assert!(bf.jmp_back(0).is_ok());
        assert_eq!(bf.data_ptr, 0);
        assert_eq!(bf.inst_ptr, 0);
    }

    #[test]
    fn test_load_program() {
        let mut bf = BfMachine::default();

        assert!(bf.load_program("").is_ok());
        assert_eq!(bf.instructions.len(), 0);

        let program: &str = "[nice comment]";
        assert!(bf.load_program(program).is_ok());
        assert_eq!(
            bf.instructions.len(),
            program.split_whitespace().collect::<String>().len()
        );

        let program: &str = "[unfinished block";
        assert!(bf.load_program(program).is_err());
    }

    #[test]
    fn test_hello_world() {
        let mut bf = BfMachine::default();
        let program = "[Hello world in brainfuck]
        ++++++++++[>+>+++>+++++++>++++++++++<<<<-]>>>++.>+.+++++++..+++.<<++.>+++++++++++++++.>.+++.------.--------.<<+.<.";
        let mut input = "".as_bytes();
        let mut output: Vec<u8> = Vec::new();
        assert!(bf.run(&mut input, &mut output, program).is_ok());
        assert_eq!(input, "".as_bytes());
        assert_eq!(output, "Hello World!\n".as_bytes());
    }

    #[test]
    fn test_hello_echo() {
        let mut bf = BfMachine::default();
        let program = ",[.,]";

        let to_echo = "abc\ndef - g";
        let mut input = to_echo.as_bytes();
        let mut output: Vec<u8> = Vec::new();
        assert!(bf.run(&mut input, &mut output, program).is_ok());
        assert_eq!(input, "".as_bytes());
        assert_eq!(output, to_echo.as_bytes());
    }

    #[test]
    fn test_squares() {
        let mut bf = BfMachine::default();
        let program = "[Outputs square numbers from 0 to 10000.
        Daniel B Cristofani (cristofdathevanetdotcom)
        http://www.hevanet.com/cristofd/brainfuck/]
        ++++[>+++++<-]>[<+++++>-]+<+[
            >[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+
            >>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]
            <<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-
        ]";

        let squares = (0..=100)
            .map(|x| (x * x).to_string())
            .collect::<Vec<String>>()
            .join("\n")
            + "\n";

        let mut output: Vec<u8> = Vec::new();
        assert!(bf.run(&mut "".as_bytes(), &mut output, program).is_ok());
        assert_eq!(output, squares.as_bytes());
    }

    #[test]
    fn test_fib() {
        let mut bf = BfMachine::default();
        bf.max_steps = Some(2148); // fib(0..13)
        let program = ">++++++++++>+>+[
            [+++++[>++++++++<-]>.<++++++[>--------<-]+<<<]>.>>[
                [-]<[>+<-]>>[<<+>+>-]<[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-
                    [>+<-[>+<-[>+<-[>[-]>+>+<<<-[>+<-]]]]]]]]]]]+>>>
            ]<<<
        ]";

        let mut output: Vec<u8> = Vec::new();
        assert!(bf.run(&mut "".as_bytes(), &mut output, program).is_ok());
    }

    #[test]
    fn test_golden() {
        let mut bf = BfMachine::default();
        bf.max_steps = Some(836262);
        let program = "[golden.b -- compute golden ratio
        (c) 2019 Daniel B. Cristofani
        http://brainfuck.org/]
        +>>>>>>>++>+>+>+>++<[
            +[
                --[++>>--]->--[
                    +[
                        +<+[-<<+]++<<[-[->-[>>-]++<[<<]++<<-]+<<]>>>>-<<<<
                        <++<-<<++++++[<++++++++>-]<.---<[->.[-]+++++>]>[[-]>>]
                    ]+>>--
                ]+<+[-<+<+]++>>
            ]<<<<[[<<]>>[-[+++<<-]+>>-]++[<<]<<<<<+>]
            >[->>[[>>>[>>]+[-[->>+>>>>-[-[+++<<[-]]+>>-]++[<<]]+<<]<-]<]]>>>>>>>
        ]";

        let mut output: Vec<u8> = Vec::new();
        assert!(bf.run(&mut "".as_bytes(), &mut output, program).is_ok());
        assert_eq!(
            output,
            "1.61803398874989484820458683436563811772030917980".as_bytes()
        );
    }
}
