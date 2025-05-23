
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