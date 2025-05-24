use std::fmt;


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol<'i> {
    name: &'i str,
}

impl<'i> From<&'i str> for Symbol<'i> {
    fn from(name: &'i str) -> Self {
        Symbol {name}
    }
}

impl<'i> Into<String> for Symbol<'i> {
    fn into(self) -> String {
        String::from(self.name)
    }
}

impl fmt::Display for Symbol<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.name.fmt(f)
    }
}