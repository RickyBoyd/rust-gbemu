use std;
use mmu::Mmu;

struct Flag {
    val: bool,
}

impl Flag {
    pub fn new() -> Flag {
        Flag { val: false }
    }

    fn set(&mut self) {
        self.val = true;
    }

    fn unset(&mut self) {
        self.val = false;
    }

    fn toggle(&mut self) {
        self.val = !self.val;
    }
}

struct Flags {
    zero: Flag,
    operation: Flag,
    half_carry: Flag,
    carry: Flag,
}

impl Flags {
    pub fn new() -> Flags {
        Flags {
            zero: Flag::new(),
            operation: Flag::new(),
            half_carry: Flag::new(),
            carry: Flag::new(),
        }
    }

    fn reset(&mut self) {
        self.zero.unset();
        self.operation.unset();
        self.half_carry.unset();
        self.carry.unset();
    }
}

struct Registers {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,

    pc: u16,
    sp: u16,

    flags: Flags,
}

impl Registers {
    
    pub fn new() -> Registers {
        Registers {
            a: 0,
            b: 0,
            c: 0,
            d: 0,
            e: 0,
            h: 0,
            l: 0,
            pc: 0,
            sp: 0,
            flags: Flags::new(),
        }
    }


    fn bc(&self) -> u16 {
        ((self.b as u16) << 8 ) + (self.c as u16)
    }

    fn de(&self) -> u16 {
        ((self.d as u16) << 8) + (self.e as u16)
    }

    fn hl(&self) -> u16 {
        ((self.h as u16) << 8) + (self.l as u16)
    }

    fn set_bc(&mut self, v: u16) {
        self.c = v as u8;
        self.b = (v >> 8) as u8;
    }

    fn set_de(&mut self, v: u16) {
        self.e = v as u8;
        self.d = (v >> 8) as u8;
    }

    fn set_hl(&mut self, v: u16) {
        self.l = v as u8;
        self.h = (v >> 8) as u8;
    }
}

pub struct Cpu {
    registers: Registers,
}

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            registers: Registers::new(),
        }
    }

    fn add8(&mut self, x: u8, y: u8) -> u8 {
        let (r, overflowed) = x.overflowing_add(y);
        self.registers.flags.carry.val = overflowed;
        r
    }

    fn next_instuction_byte(&mut self, mmu: &Mmu) -> u8 {
        let inst = mmu.read_byte(self.registers.pc);
        self.registers.pc += 1;
        inst
    }

    fn execute_cycle(&mut self, mmu: &Mmu) {
        self.execute_instruction(mmu);
        //Now deal with time
    }

    fn execute_instruction(&mut self, mmu: &Mmu) {
        let opcode = self.next_instuction_byte(mmu);

        macro_rules! xor (
        ($val:expr) => ({
            self.registers.a ^= $val;
            self.registers.flags.reset();
            self.registers.flags.zero.val = self.registers.a == 0;
        }) );

        macro_rules! and (
        ($val:expr) => ({
            self.registers.a &= $val;
            self.registers.flags.reset();
            self.registers.flags.zero.val = self.registers.a == 0;
        }) );

        macro_rules! or (
        ($val:expr) => ({
            self.registers.a ^= $val;
            self.registers.flags.reset();
            self.registers.flags.zero.val = self.registers.a == 0;
        }) );

        macro_rules! inc (
        ($val:expr) => ({
            $val = $val.wrapping_add(1);
        }) );

        macro_rules! dec (
        ($val:expr) => ({
            $val = $val.wrapping_sub(1);
        }) );
        
        match opcode {
            //NOP
            0x00 => {  }
            //LD BC,d16
            0x01 => {  }
            //LD (BC),A
            0x02 => {  }
            //INC BC
            0x03 => { let bc = self.registers.bc(); self.registers.set_bc(bc.wrapping_add(1)); }
            //INC B
            0x04 => { inc!(self.registers.b); }
            //DEC B
            0x05 => { dec!(self.registers.b); }
            //LD B,d8
            0x06 => {  }
            //RLCA
            0x07 => {  }
            //LD (a16),SP
            0x08 => {  }
            //ADD HL,BC
            0x09 => {  }
            //LD A,(BC)
            0x0A => {  }
            //DEC BC
            0x0B => {  }
            //INC C
            0x0C => { inc!(self.registers.c); }
            //DEC C
            0x0D => { dec!(self.registers.c); }
            //LD C,d8
            0x0E => {  }
            //RRCA
            0x0F => {  }
            //STOP 0
            0x10 => {  }
            //LD DE,d16
            0x11 => {  }
            //LD (DE),A
            0x12 => {  }
            //INC DE
            0x13 => {  }
            //INC D
            0x14 => { inc!(self.registers.d); }
            //DEC D
            0x15 => { dec!(self.registers.d); }
            //LD D,d8
            0x16 => {  }
            //RLA
            0x17 => {  }
            //JR r8
            0x18 => {  }
            //ADD HL,DE
            0x19 => {  }
            //LD A,(DE)
            0x1A => {  }
            //DEC DE
            0x1B => {  }
            //INC E
            0x1C => { inc!(self.registers.e); }
            //DEC E
            0x1D => { dec!(self.registers.e); }
            //LD E,d8
            0x1E => {  }
            //RRA
            0x1F => {  }
            //JR NZ,r8
            0x20 => {  }
            //LD HL,d16
            0x21 => {  }
            //LD (HL+),A
            0x22 => {  }
            //INC HL
            0x23 => {  }
            //INC H
            0x24 => { inc!(self.registers.h); }
            //DEC H
            0x25 => { dec!(self.registers.h); }
            //LD H,d8
            0x26 => {  }
            //DAA
            0x27 => {  }
            //JR Z,r8
            0x28 => {  }
            //ADD HL,HL
            0x29 => {  }
            //LD A,(HL+)
            0x2A => {  }
            //DEC HL
            0x2B => {  }
            //INC L
            0x2C => { inc!(self.registers.l); }
            //DEC L
            0x2D => { dec!(self.registers.l); }
            //LD L,d8
            0x2E => {  }
            //CPL
            0x2F => {  }
            //JR NC,r8
            0x30 => {  }
            //LD SP,d16
            0x31 => {  }
            //LD (HL-),A
            0x32 => {  }
            //INC SP
            0x33 => {  }
            //INC (HL)
            0x34 => {  }
            //DEC (HL)
            0x35 => {  }
            //LD (HL),d8
            0x36 => {  }
            //SCF
            0x37 => { self.registers.flags.carry.set() }
            //JR C,r8
            0x38 => {  }
            //ADD HL,SP
            0x39 => {  }
            //LD A,(HL-)
            0x3A => {  }
            //DEC SP
            0x3B => {  }
            //INC A
            0x3C => { inc!(self.registers.a); }
            //DEC A
            0x3D => { dec!(self.registers.a); }
            //LD A,d8
            0x3E => {  }
            //CCF
            0x3F => { self.registers.flags.carry.toggle() }
            //LD B,B
            0x40 => {  }
            //LD B,C
            0x41 => { self.registers.b = self.registers.c; }
            //LD B,D
            0x42 => { self.registers.b = self.registers.d; }
            //LD B,E
            0x43 => { self.registers.b = self.registers.e; }
            //LD B,H
            0x44 => { self.registers.b = self.registers.h; }
            //LD B,L
            0x45 => { self.registers.b = self.registers.l; }
            //LD B,(HL)
            0x46 => {  }
            //LD B,A
            0x47 => { self.registers.b = self.registers.a; }
            //LD C,B
            0x48 => { self.registers.c = self.registers.b; }
            //LD C,C
            0x49 => {  }
            //LD C,D
            0x4A => { self.registers.c = self.registers.d; }
            //LD C,E
            0x4B => { self.registers.c = self.registers.e; }
            //LD C,H
            0x4C => { self.registers.c = self.registers.h; }
            //LD C,L
            0x4D => { self.registers.c = self.registers.l; }
            //LD C,(HL)
            0x4E => {  }
            //LD C,A
            0x4F => { self.registers.c = self.registers.a; }
            //LD D,B
            0x50 => { self.registers.d = self.registers.b; }
            //LD D,C
            0x51 => { self.registers.d = self.registers.c; }
            //LD D,D
            0x52 => { }
            //LD D,E
            0x53 => { self.registers.d = self.registers.e; }
            //LD D,H
            0x54 => { self.registers.d = self.registers.h; }
            //LD D,L
            0x55 => { self.registers.d = self.registers.l; }
            //LD D,(HL)
            0x56 => {   }
            //LD D,A
            0x57 => { self.registers.d = self.registers.a; }
            //LD E,B
            0x58 => { self.registers.e = self.registers.b; }
            //LD E,C
            0x59 => { self.registers.e = self.registers.c; }
            //LD E,D
            0x5A => { self.registers.e = self.registers.d; }
            //LD E,E
            0x5B => {  }
            //LD E,H
            0x5C => { self.registers.e = self.registers.h; }
            //LD E,L
            0x5D => { self.registers.e = self.registers.l; }
            //LD E,(HL)
            0x5E => {   }
            //LD E,A
            0x5F => { self.registers.e = self.registers.a; }
            //LD H,B
            0x60 => { self.registers.h = self.registers.b; }
            //LD H,C
            0x61 => { self.registers.h = self.registers.c; }
            //LD H,D
            0x62 => { self.registers.h = self.registers.d; }
            //LD H,E
            0x63 => { self.registers.h = self.registers.e; }
            //LD H,H
            0x64 => {  }
            //LD H,L
            0x65 => { self.registers.h = self.registers.l; }
            //LD H,(HL)
            0x66 => { self.registers.h = self.registers.c; }
            //LD H,A
            0x67 => { self.registers.h = self.registers.a; }
            //LD L,B
            0x68 => { self.registers.l = self.registers.b; }
            //LD L,C
            0x69 => { self.registers.l = self.registers.c; }
            //LD L,D
            0x6A => { self.registers.l = self.registers.d; }
            //LD L,E
            0x6B => { self.registers.l = self.registers.e; }
            //LD L,H
            0x6C => { self.registers.l = self.registers.h; }
            //LD L,L
            0x6D => {  }
            //LD L,(HL)
            0x6E => {  }
            //LD L,A
            0x6F => { self.registers.l = self.registers.a; }
            //LD (HL),B
            0x70 => {  }
            //LD (HL),C
            0x71 => {  }
            //LD (HL),D
            0x72 => {  }
            //LD (HL),E
            0x73 => {  }
            //LD (HL),H
            0x74 => {  }
            //LD (HL),L
            0x75 => {  }
            //HALT
            0x76 => {  }
            //LD (HL),A
            0x77 => {  }
            //LD A,B
            0x78 => { self.registers.a = self.registers.b; }
            //LD A,C
            0x79 => { self.registers.a = self.registers.c; }
            //LD A,D
            0x7A => { self.registers.a = self.registers.d; }
            //LD A,E
            0x7B => { self.registers.a = self.registers.d; }
            //LD A,H
            0x7C => { self.registers.a = self.registers.h; }
            //LD A,L
            0x7D => { self.registers.a = self.registers.l; }
            //LD A,(HL)
            0x7E => { self.registers.a = self.registers.d; }
            //LD A,A
            0x7F => {  }
            //ADD A,B
            0x80 => { self.registers.a = self.registers.b; }
            //ADD A,C
            0x81 => { self.registers.a = self.registers.c; }
            //ADD A,D
            0x82 => { self.registers.a = self.registers.d; }
            //ADD A,E
            0x83 => { self.registers.a = self.registers.e; }
            //ADD A,H
            0x84 => { self.registers.a = self.registers.h; }
            //ADD A,L
            0x85 => { self.registers.a = self.registers.l; }
            //ADD A,(HL)
            0x86 => {  }
            //ADD A,A
            0x87 => {  }
            //ADC A,B
            0x88 => { self.registers.a = self.registers.b; }
            //ADC A,C
            0x89 => { self.registers.a = self.registers.c; }
            //ADC A,D
            0x8A => { self.registers.a = self.registers.d; }
            //ADC A,E
            0x8B => { self.registers.a = self.registers.e; }
            //ADC A,H
            0x8C => { self.registers.a = self.registers.h; }
            //ADC A,L
            0x8D => { self.registers.a = self.registers.l; }
            //ADC A,(HL)
            0x8E => {  }
            //ADC A,A
            0x8F => {  }
            //SUB B
            0x90 => {  }
            //SUB C
            0x91 => {  }
            //SUB D
            0x92 => {  }
            //SUB E
            0x93 => {  }
            //SUB H
            0x94 => {  }
            //SUB L
            0x95 => {  }
            //SUB (HL)
            0x96 => {  }
            //SUB A
            0x97 => {  }
            //SBC A,B
            0x98 => {  }
            //SBC A,C
            0x99 => {  }
            //SBC A,D
            0x9A => {  }
            //SBC A,E
            0x9B => {  }
            //SBC A,H
            0x9C => {  }
            //SBC A,L
            0x9D => {  }
            //SBC A,(HL)
            0x9E => {  }
            //SBC A,A
            0x9F => {  }
            //AND B
            0xA0 => { and!(self.registers.b); }
            //AND C
            0xA1 => { and!(self.registers.c); }
            //AND D
            0xA2 => { and!(self.registers.d); }
            //AND E
            0xA3 => { and!(self.registers.e); }
            //AND H
            0xA4 => { and!(self.registers.h); }
            //AND L
            0xA5 => { and!(self.registers.l); }
            //AND (HL)
            0xA6 => {  }
            //AND A
            0xA7 => { and!(self.registers.a); }
            //XOR B
            0xA8 => { xor!(self.registers.b); }
            //XOR C
            0xA9 => { xor!(self.registers.c); }
            //XOR D
            0xAA => { xor!(self.registers.d); }
            //XOR E
            0xAB => { xor!(self.registers.e); }
            //XOR H
            0xAC => { xor!(self.registers.h); }
            //XOR L
            0xAD => { xor!(self.registers.l); }
            //XOR (HL)
            0xAE => {  }
            //XOR A
            0xAF => { xor!(self.registers.a); }
            //OR B
            0xB0 => { or!(self.registers.b);  }
            //OR C
            0xB1 => { or!(self.registers.c); }
            //OR D
            0xB2 => { or!(self.registers.d); }
            //OR E
            0xB3 => { or!(self.registers.e); }
            //OR H
            0xB4 => { or!(self.registers.h); }
            //OR L
            0xB5 => { or!(self.registers.l); }
            //OR (HL)
            0xB6 => {  }
            //OR A
            0xB7 => { or!(self.registers.a); }
            //CP B
            0xB8 => {  }
            //CP C
            0xB9 => {  }
            //CP D
            0xBA => {  }
            //CP E
            0xBB => {  }
            //CP H
            0xBC => {  }
            //CP L
            0xBD => {  }
            //CP (HL)
            0xBE => {  }
            //CP A
            0xBF => {  }
            //RET NZ
            0xC0 => {  }
            //POP BC
            0xC1 => {  }
            //JP NZ,a16
            0xC2 => {  }
            //JP a16
            0xC3 => {  }
            //CALL NZ,a1
            0xC4 => {  }
            //PUSH BC
            0xC5 => {  }
            //ADD A,d8
            0xC6 => {  }
            //RST 00H
            0xC7 => {  }
            //RET Z
            0xC8 => {  }
            //RET
            0xC9 => {  }
            //JP Z,a16
            0xCA => {  }
            //PREFIX CB
            0xCB => {  }
            _ => { println!("Instruction {} unimplemented", opcode); }
        }
    }
}