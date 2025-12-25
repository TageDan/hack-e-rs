#![no_std]

extern crate alloc;

use core::fmt::Debug;

use alloc::boxed::Box;

/// The hack-e-rs crate provides a implementation for a hack-computer virtual machine described in the course nand2tetris.
/// It provides two modules:
///  * A VM module which contains the implementation of the VM struct.
///  * And a MMIO module which provides the MMIO trait which can be used to extend the VM with custom
/// "hardware", essentially hooking a memory range up to some custom made driver.
///
/// This crate will then be used as a library to create any virtual device you can imagine centered around the hack cpu.
///
/// Note: No MMIO for SCREEN or KBD as defined in nand2tetris is defined in this crate. By default the whole memory is dedicated to RAM.
/// Note: this library uses no_std and therefore could be used on embedded devices

/// Main VM struct. Used togheter with MMIO types to create "devices" and to run ".hack" files.
pub mod mmio;

/// len of memory with 15-bit addresses
const MEM_LEN: usize = 0b1000_0000_0000_0000;

pub struct VM {
    pc: u16,
    rom: [Instruction; MEM_LEN],
    ram: [u16; MEM_LEN],
    // memory mapped io's
    mmio: Box<[Box<dyn mmio::MMIO>]>,
    a_reg: u16,
    d_reg: u16,
}

impl VM {
    /// create a new VM with specified optional list of mmio's
    pub fn new(mmio: Option<Box<[Box<dyn mmio::MMIO>]>>) -> Self {
        Self {
            pc: 0,
            // empty instruction
            rom: [Instruction::new(0b0); MEM_LEN],
            ram: [0; MEM_LEN],
            mmio: mmio.unwrap_or(Box::new([])),
            a_reg: 0,
            d_reg: 0,
        }
    }

    /// Load program into rom (extend program with zeroes in order to overwrite the rest of the memory)
    pub fn load(&mut self, program: &[u16]) {
        program
            .iter()
            .chain((0..).map(|_| &0))
            .zip(self.rom.iter_mut())
            .for_each(|(x, y)| *y = Instruction(*x));
    }

    /// run a single instruction
    pub fn step(&mut self) {
        let instr = self.rom[self.pc as usize];

        let (res, dest, jump) = instr.exec(self.a_reg, self.d_reg, self.read_ram());

        if jump {
            self.pc = self.a_reg;
        } else {
            self.pc += 1;
        }

        match dest {
            x if x.m() => self.write_ram(res),
            x if x.a() => self.a_reg = res,
            x if x.d() => self.d_reg = res,
            _ => (),
        }
    }

    /// reset the vm to restart the program
    pub fn reset(&mut self) {
        self.pc = 0;
        self.ram = [0; MEM_LEN];
        self.mmio.iter_mut().for_each(|x| x.reset());
        self.a_reg = 0;
        self.d_reg = 0;
    }

    /// get program rom
    pub fn get_rom(&self) -> &[Instruction; MEM_LEN] {
        &self.rom
    }

    fn read_ram(&self) -> u16 {
        for mmio in self.mmio.iter() {
            if mmio.range().contains(&self.a_reg) {
                return mmio.read(mmio.inner_address(self.a_reg));
            }
        }
        return self.ram[self.a_reg as usize];
    }

    fn write_ram(&mut self, val: u16) {
        for mmio in self.mmio.iter_mut() {
            if mmio.range().contains(&self.a_reg) {
                mmio.write(mmio.inner_address(self.a_reg), val);
                return;
            }
        }
        self.ram[self.a_reg as usize] = val;
    }
}

/// Single instruction
#[derive(Clone, Copy)]
pub struct Instruction(u16);

impl Instruction {
    /// Create instruction from binary representation
    fn new(inst: u16) -> Self {
        Self(inst)
    }

    /// execute the instruction with the given values for a and d registers and the value m = RAM[a]
    /// Outputs a result of the operation (u16), dest (Destination) and jump (bool) (res, dest, jump)
    fn exec(&self, a: u16, d: u16, m: u16) -> (u16, Destination, bool) {
        let val = if let Some(addr) = self.addr() {
            addr
        } else {
            // if a bit is set use m else use a
            let y = match self.a_bit() {
                true => m,
                false => a,
            };

            if let Some(comp) = self.comp() {
                comp.exec(d, y)
            } else {
                // this should never happen (both self.comp() and self.addr() should never be None)
                panic!("NO COMPUTATION OR ADDRESS???")
            }
        };

        let dest = self.dest();
        let jmp = if let Some(jump) = self.jmp() {
            jump.check(val)
        } else {
            false
        };

        (val, dest, jmp)
    }

    /// Returns the Computation of the c-instruction (None for a-instructions)
    fn comp(&self) -> Option<Computation> {
        match self.is_c_instruction() {
            // comp bits
            true => Some(Computation(((self.0 >> 6) & 0b00111111) as u8)),
            false => None,
        }
    }

    /// Returns jump for c-instruction (None for a-instruction)
    fn jmp(&self) -> Option<Jump> {
        match self.is_c_instruction() {
            true => Some(Jump::from(self.0 & 0b111)),
            false => None,
        }
    }

    /// Returns destination for c-instruction, Destination(1,0,0) for a-instruction (a-reg)
    fn dest(&self) -> Destination {
        match self.is_c_instruction() {
            true => Destination::from((self.0 >> 3) & 0b111),
            // a-register
            false => Destination::from(0b100),
        }
    }

    /// Returns the address to be stored in the A register (a-instruction), None for c-instruction
    fn addr(&self) -> Option<u16> {
        match self.is_a_instruction() {
            true => Some(self.0),
            false => None,
        }
    }

    /// returns if the 'a bit' is set. Determins if A or M should be used as y.
    fn a_bit(&self) -> bool {
        // bit 12 is set
        (self.0 >> 12) & 0b1 == 1
    }

    fn is_c_instruction(&self) -> bool {
        // first bit is one
        (self.0 >> 15) & 0b1 == 1
    }

    fn is_a_instruction(&self) -> bool {
        // first bit is zero
        (self.0 >> 15) & 0b1 == 0
    }
}

/// Computation ('0','0',c1,c2,c3,c4,c5,c6)
struct Computation(u8);

impl From<u16> for Computation {
    fn from(value: u16) -> Self {
        Computation((value & 0b0011_1111) as u8)
    }
}

impl Computation {
    /// execute computation given x (d) and y (a|m). Panics if this computation is invalid.
    /// See page 67: https://github.com/jherskow/nand2tetris/blob/master/nand2tetris%20BOOK.pdf
    /// TODO: Return a result in constructor instead.
    fn exec(&self, x: u16, y: u16) -> u16 {
        match self.0 {
            0b101010 => 0,
            0b111111 => 1,
            0b111010 => u16::MAX, // 0b111... (-1)
            0b001100 => x,
            0b110000 => y,
            0b001101 => !x,
            0b110001 => !y,
            0b001111 => x.wrapping_neg(),
            0b110011 => y.wrapping_neg(),
            0b011111 => x + 1,
            0b110111 => y + 1,
            0b001110 => x - 1,
            0b110010 => y - 1,
            0b000010 => x + y,
            0b010011 => x - y,
            0b000111 => y - x,
            0b000000 => x & y,
            0b010101 => y | x,
            _ => panic!("INVALID COMPUTATION!"),
        }
    }

    fn comp_str(&self) -> &'static str {
        match self.0 {
            0b101010 => "0",
            0b111111 => "1",
            0b111010 => "-1",
            0b001100 => "x",
            0b110000 => "y",
            0b001101 => "!x",
            0b110001 => "!y",
            0b001111 => "-x",
            0b110011 => "-y",
            0b011111 => "x+1",
            0b110111 => "y+1",
            0b001110 => "x-1",
            0b110010 => "y-1",
            0b000010 => "x+y",
            0b010011 => "x-y",
            0b000111 => "y-x",
            0b000000 => "x&y",
            0b010101 => "y|x",
            _ => panic!("INVALID COMPUTATION!"),
        }
    }
}

/// Destination (A,D,M)
struct Destination(bool, bool, bool);

impl From<u16> for Destination {
    fn from(value: u16) -> Self {
        Destination(value & 0b100 != 0, value & 0b010 != 0, value & 0b001 != 0)
    }
}

impl Destination {
    fn a(&self) -> bool {
        self.0
    }
    fn d(&self) -> bool {
        self.1
    }
    fn m(&self) -> bool {
        self.2
    }
}

/// Jump (LT, EQ, GT)
struct Jump(bool, bool, bool);

impl From<u16> for Jump {
    fn from(value: u16) -> Self {
        Jump(value & 0b100 != 0, value & 0b010 != 0, value & 0b001 != 0)
    }
}

impl Jump {
    fn lt(&self) -> bool {
        self.0
    }
    fn eq(&self) -> bool {
        self.1
    }
    fn gt(&self) -> bool {
        self.2
    }

    fn check(&self, val: u16) -> bool {
        // highest bit set => val < 0
        let lt = self.lt() && (val & (1 << 15)) == 1;
        let gt = self.gt() && val > 0;
        let eq = self.eq() && val == 0;

        // if any jump condition is true, jump
        lt || eq || gt
    }
}

impl Debug for Instruction {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_str("\r\n| comp | | dest | | jump |\r\n")?;
        f.write_fmt(format_args!(
            "|{: ^5?}| |{: ^5?}| |{: ^5?}|\r\n",
            self.comp(),
            self.dest(),
            self.jmp()
        ))
    }
}

impl Debug for Computation {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_fmt(format_args!("{}", self.comp_str()))
    }
}

impl Debug for Jump {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let res = match (self.lt(), self.eq(), self.gt()) {
            (false, false, false) => "no",
            (true, true, true) => "yes",
            (true, false, false) => "lt",
            (false, true, false) => "eq",
            (false, false, true) => "gt",
            (true, true, false) => "le",
            (true, false, true) => "ne",
            (false, true, true) => "ge",
        };
        f.write_str(res)
    }
}

impl Debug for Destination {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let res = match (self.a(), self.d(), self.m()) {
            (false, false, false) => "none",
            (true, true, true) => "AMD",
            (true, false, false) => "A",
            (false, true, false) => "D",
            (false, false, true) => "M",
            (true, true, false) => "AD",
            (true, false, true) => "AM",
            (false, true, true) => "DM",
        };
        f.write_str(res)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_instruction() {
        // @467
        let x = Instruction::new(0b0000000111010011);

        assert!(x.is_a_instruction());

        assert!(!x.is_c_instruction());

        let res = x.exec(19, 67, 190);
        assert!(res.0 == 467);
        assert!(res.1.a());
        assert!(!res.1.d());
        assert!(!res.1.m());
        assert!(!res.2);
    }
    #[test]
    fn c_instruction() {
        // DM=D-A;JGE
        let x = Instruction::new(0b1110010011011011);

        assert!(!x.is_a_instruction());

        assert!(x.is_c_instruction());

        let res = x.exec(60, 67, 198);
        assert!(res.0 == 7);
        assert!(!res.1.a());
        assert!(res.1.d());
        assert!(res.1.m());
        assert!(res.2);
    }
}
