pub trait Domain: std::fmt::Debug + std::fmt::Display + Clone {
    type Element;

    fn elements() -> &'static [Self::Element];
}

impl Domain for bool {
    type Element = bool;

    fn elements() -> &'static [Self::Element] {
        static ELEMENTS: [bool; 2] = [true, false];

        &ELEMENTS
    }
}
