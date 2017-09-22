const WRAM_SIZE: usize = 16000;

pub struct Mmu {
    wram: [u8; WRAM_SIZE],
}

impl Mmu {
    pub fn write_byte(&self, addr: u16) {

    }

    pub fn write_word(&self, addr: u16) {
        
    }

    pub fn read_byte(&self, addr: u16) -> u8 {
        0
    }

    pub fn read_word(&self, addr: u16) -> u16 {
        0
    }
}