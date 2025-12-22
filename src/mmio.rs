pub trait MMIO {
    /// Return lower address bound of the interface
    fn lower_bound(&self) -> u16;
    /// Return upper address bound of the interface
    fn upper_bound(&self) -> u16;

    /// Returns address range for the interface
    fn range(&self) -> core::ops::RangeInclusive<u16> {
        self.lower_bound()..=self.upper_bound()
    }

    /// Returns address - lower address bound
    fn inner_address(&self, address: u16) -> u16 {
        address
            .checked_sub(self.lower_bound())
            .expect("Address to low for interface")
    }

    /// Defines behavior for writing into the memory map. (address is relative to self.lower_bound())
    fn write(&mut self, val: u16, address: u16);

    /// Defines behavior for reading from the memory map. (address is relative to self.lower_bound())
    fn read(&mut self, address: u16) -> u16;

    /// Reset the mmio interface (called for all interfaces when vm is reset)
    fn reset(&mut self);
}
