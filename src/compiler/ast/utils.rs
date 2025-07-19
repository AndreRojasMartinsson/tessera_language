use crate::node;

use bumpalo::collections::Vec;

node!(Punctuated<T>, {
    nodes: Vec<'a, T>
});
