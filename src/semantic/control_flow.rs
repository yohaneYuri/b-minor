pub trait ReturnGuaranteed {
    fn must_return(&self) -> bool;
}