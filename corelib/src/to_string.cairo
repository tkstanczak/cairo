use byte_array::ByteArrayTrait;
use traits::{Into, TryInto};
use option::OptionTrait;
use zeroable::Zeroable;

// === Hexadecimal base ===

impl U8IntoHexAsciiByteArray of Into<u8, ByteArray> {
    fn into(self: u8) -> ByteArray {
        to_byte_array_hex(self, 16_u8.try_into().unwrap())
    }
}
impl U16IntoHexAsciiByteArray of Into<u16, ByteArray> {
    fn into(self: u16) -> ByteArray {
        to_byte_array_hex(self, 16_u16.try_into().unwrap())
    }
}
impl U32IntoHexAsciiByteArray of Into<u32, ByteArray> {
    fn into(self: u32) -> ByteArray {
        to_byte_array_hex(self, 16_u32.try_into().unwrap())
    }
}
impl U64IntoHexAsciiByteArray of Into<u64, ByteArray> {
    fn into(self: u64) -> ByteArray {
        to_byte_array_hex(self, 16_u64.try_into().unwrap())
    }
}
impl U128IntoHexAsciiByteArray of Into<u128, ByteArray> {
    fn into(self: u128) -> ByteArray {
        to_byte_array_hex(self, 16_u128.try_into().unwrap())
    }
}
impl U256IntoHexAsciiByteArray of Into<u256, ByteArray> {
    fn into(self: u256) -> ByteArray {
        to_byte_array_hex(self, 16_u256.try_into().unwrap())
    }
}
impl Felt252IntoHexAsciiByteArray of Into<felt252, ByteArray> {
    fn into(mut self: felt252) -> ByteArray {
        let as_u256: u256 = self.into();
        U256IntoHexAsciiByteArray::into(as_u256)
    }
}

// TODO(yuval): once const generic parameters are supported, move hex_base_nz to be a generic
// parameter.
/// Formats a type that behaves like uint into a hexadecimal string.
fn to_byte_array_hex<
    T,
    impl DropImpl: Drop<T>,
    impl CopyImpl: Copy<T>,
    impl DivRemImpl: DivRem<T>,
    impl TryIntoU8: TryInto<T, u8>,
    impl ZeroableImpl: Zeroable<T>,
>(
    mut value: T, hex_base_nz: NonZero<T>,
) -> ByteArray {
    let mut result: ByteArray = "";
    loop {
        let (new_value, digit) = DivRem::div_rem(value, hex_base_nz);
        value = new_value;
        let digit_as_u8: u8 = digit.try_into().unwrap();
        result
            .append_byte(if digit_as_u8 < 10 {
                digit_as_u8 + '0'
            } else {
                digit_as_u8 - 10 + 'A'
            });
        if value.is_zero() {
            break;
        };
    };
    "0x" + result.rev()
}

// === Decimal base ===

impl U8IntoDecAsciiByteArray of Into<u8, ByteArray> {
    fn into(self: u8) -> ByteArray {
        to_byte_array_dec(self, 10_u8.try_into().unwrap())
    }
}
impl U16IntoDecAsciiByteArray of Into<u16, ByteArray> {
    fn into(self: u16) -> ByteArray {
        to_byte_array_dec(self, 10_u16.try_into().unwrap())
    }
}
impl U32IntoDecAsciiByteArray of Into<u32, ByteArray> {
    fn into(self: u32) -> ByteArray {
        to_byte_array_dec(self, 10_u32.try_into().unwrap())
    }
}
impl U64IntoDecAsciiByteArray of Into<u64, ByteArray> {
    fn into(self: u64) -> ByteArray {
        to_byte_array_dec(self, 10_u64.try_into().unwrap())
    }
}
impl U128IntoDecAsciiByteArray of Into<u128, ByteArray> {
    fn into(self: u128) -> ByteArray {
        to_byte_array_dec(self, 10_u128.try_into().unwrap())
    }
}
impl U256IntoDecAsciiByteArray of Into<u256, ByteArray> {
    fn into(self: u256) -> ByteArray {
        to_byte_array_dec(self, 10_u256.try_into().unwrap())
    }
}
impl Felt252IntoDecAsciiByteArray of Into<felt252, ByteArray> {
    fn into(mut self: felt252) -> ByteArray {
        let as_u256: u256 = self.into();
        U256IntoDecAsciiByteArray::into(as_u256)
    }
}

/// Formats a type that behaves like uint into a decimal string.
fn to_byte_array_dec<
    T,
    impl DropImpl: Drop<T>,
    impl CopyImpl: Copy<T>,
    impl DivRemImpl: DivRem<T>,
    impl TryIntoU8: TryInto<T, u8>,
    impl ZeroableImpl: Zeroable<T>,
>(
    mut value: T, dec_base_nz: NonZero<T>,
) -> ByteArray {
    let mut result: ByteArray = "";
    loop {
        let (new_value, digit) = DivRem::div_rem(value, dec_base_nz);
        value = new_value;
        let digit_as_u8: u8 = digit.try_into().unwrap();
        result.append_byte(digit_as_u8 + '0');
        if value.is_zero() {
            break;
        };
    };
    result.rev()
}

// === Octal base ===

impl U8IntoOctAsciiByteArray of Into<u8, ByteArray> {
    fn into(self: u8) -> ByteArray {
        to_byte_array_oct(self, 8_u8.try_into().unwrap())
    }
}
impl U16IntoOctAsciiByteArray of Into<u16, ByteArray> {
    fn into(self: u16) -> ByteArray {
        to_byte_array_oct(self, 8_u16.try_into().unwrap())
    }
}
impl U32IntoOctAsciiByteArray of Into<u32, ByteArray> {
    fn into(self: u32) -> ByteArray {
        to_byte_array_oct(self, 8_u32.try_into().unwrap())
    }
}
impl U64IntoOctAsciiByteArray of Into<u64, ByteArray> {
    fn into(self: u64) -> ByteArray {
        to_byte_array_oct(self, 8_u64.try_into().unwrap())
    }
}
impl U128IntoOctAsciiByteArray of Into<u128, ByteArray> {
    fn into(self: u128) -> ByteArray {
        to_byte_array_oct(self, 8_u128.try_into().unwrap())
    }
}
impl U256IntoOctAsciiByteArray of Into<u256, ByteArray> {
    fn into(self: u256) -> ByteArray {
        to_byte_array_oct(self, 8_u256.try_into().unwrap())
    }
}
impl Felt252IntoOctAsciiByteArray of Into<felt252, ByteArray> {
    fn into(mut self: felt252) -> ByteArray {
        let as_u256: u256 = self.into();
        U256IntoOctAsciiByteArray::into(as_u256)
    }
}

/// Formats a type that behaves like uint into an octal string.
fn to_byte_array_oct<
    T,
    impl DropImpl: Drop<T>,
    impl CopyImpl: Copy<T>,
    impl DivRemImpl: DivRem<T>,
    impl TryIntoU8: TryInto<T, u8>,
    impl ZeroableImpl: Zeroable<T>,
>(
    mut value: T, dec_base_nz: NonZero<T>,
) -> ByteArray {
    let mut result: ByteArray = "";
    loop {
        let (new_value, digit) = DivRem::div_rem(value, dec_base_nz);
        value = new_value;
        let digit_as_u8: u8 = digit.try_into().unwrap();
        result.append_byte(digit_as_u8 + '0');
        if value.is_zero() {
            break;
        };
    };
    "0o" + result.rev()
}

// === Binary base ===

impl U8IntoBinAsciiByteArray of Into<u8, ByteArray> {
    fn into(self: u8) -> ByteArray {
        to_byte_array_bin(self, 2_u8.try_into().unwrap())
    }
}
impl U16IntoBinAsciiByteArray of Into<u16, ByteArray> {
    fn into(self: u16) -> ByteArray {
        to_byte_array_bin(self, 2_u16.try_into().unwrap())
    }
}
impl U32IntoBinAsciiByteArray of Into<u32, ByteArray> {
    fn into(self: u32) -> ByteArray {
        to_byte_array_bin(self, 2_u32.try_into().unwrap())
    }
}
impl U64IntoBinAsciiByteArray of Into<u64, ByteArray> {
    fn into(self: u64) -> ByteArray {
        to_byte_array_bin(self, 2_u64.try_into().unwrap())
    }
}
impl U128IntoBinAsciiByteArray of Into<u128, ByteArray> {
    fn into(self: u128) -> ByteArray {
        to_byte_array_bin(self, 2_u128.try_into().unwrap())
    }
}
impl U256IntoBinAsciiByteArray of Into<u256, ByteArray> {
    fn into(self: u256) -> ByteArray {
        to_byte_array_bin(self, 2_u256.try_into().unwrap())
    }
}
impl Felt252IntoBinAsciiByteArray of Into<felt252, ByteArray> {
    fn into(mut self: felt252) -> ByteArray {
        let as_u256: u256 = self.into();
        U256IntoBinAsciiByteArray::into(as_u256)
    }
}

/// Formats a type that behaves like uint into a binary string.
fn to_byte_array_bin<
    T,
    impl DropImpl: Drop<T>,
    impl CopyImpl: Copy<T>,
    impl DivRemImpl: DivRem<T>,
    impl TryIntoU8: TryInto<T, u8>,
    impl ZeroableImpl: Zeroable<T>,
>(
    mut value: T, dec_base_nz: NonZero<T>,
) -> ByteArray {
    let mut result: ByteArray = "";
    loop {
        let (new_value, digit) = DivRem::div_rem(value, dec_base_nz);
        value = new_value;
        let digit_as_u8: u8 = digit.try_into().unwrap();
        result.append_byte(digit_as_u8 + '0');
        if value.is_zero() {
            break;
        };
    };
    "0b" + result.rev()
}
