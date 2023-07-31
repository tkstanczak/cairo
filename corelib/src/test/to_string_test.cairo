use test::test_utils::assert_eq;
use to_string::{
    U8IntoHexAsciiByteArray, U16IntoHexAsciiByteArray, U32IntoHexAsciiByteArray,
    U64IntoHexAsciiByteArray, U128IntoHexAsciiByteArray, U256IntoHexAsciiByteArray,
    Felt252IntoHexAsciiByteArray, U8IntoDecAsciiByteArray, U16IntoDecAsciiByteArray,
    U32IntoDecAsciiByteArray, U64IntoDecAsciiByteArray, U128IntoDecAsciiByteArray,
    U256IntoDecAsciiByteArray, Felt252IntoDecAsciiByteArray, U8IntoOctAsciiByteArray,
    U16IntoOctAsciiByteArray, U32IntoOctAsciiByteArray, U64IntoOctAsciiByteArray,
    U128IntoOctAsciiByteArray, U256IntoOctAsciiByteArray, Felt252IntoOctAsciiByteArray,
    U8IntoBinAsciiByteArray, U16IntoBinAsciiByteArray, U32IntoBinAsciiByteArray,
    U64IntoBinAsciiByteArray, U128IntoBinAsciiByteArray, U256IntoBinAsciiByteArray,
    Felt252IntoBinAsciiByteArray
};
use traits::{Into, TryInto};

#[test]
#[available_gas(10000000)]
fn test_to_string_hex_u8() {
    let expected_string = "0x0";
    let serialized: ByteArray = U8IntoHexAsciiByteArray::into(0_u8);
    assert_eq(@serialized, @expected_string, 'Bad hex representation of 0');

    let expected_string = "0xA";
    let serialized: ByteArray = U8IntoHexAsciiByteArray::into(0xA_u8);
    assert_eq(@serialized, @expected_string, 'Bad hex representation of 10');

    let expected_string = "0x6F";
    let serialized: ByteArray = U8IntoHexAsciiByteArray::into(0x6F_u8);
    assert_eq(@serialized, @expected_string, 'Bad hex representation of 111');

    let expected_string = "0xFF";
    let serialized: ByteArray = U8IntoHexAsciiByteArray::into(0xFF_u8);
    assert_eq(@serialized, @expected_string, 'Bad hex representation of 255');

    // Other uint types:
    let expected_string = "0x6F";
    let serialized: ByteArray = U16IntoHexAsciiByteArray::into(0x6F_u16);
    assert_eq(@serialized, @expected_string, 'Bad u16 hex representation');
    let serialized: ByteArray = U32IntoHexAsciiByteArray::into(0x6F_u32);
    assert_eq(@serialized, @expected_string, 'Bad u32 hex representation');
    let serialized: ByteArray = U64IntoHexAsciiByteArray::into(0x6F_u64);
    assert_eq(@serialized, @expected_string, 'Bad u64 hex representation');
    let serialized: ByteArray = U128IntoHexAsciiByteArray::into(0x6F_u128);
    assert_eq(@serialized, @expected_string, 'Bad u128 hex representation');
    let serialized: ByteArray = U256IntoHexAsciiByteArray::into(0x6F_u256);
    assert_eq(@serialized, @expected_string, 'Bad u256 hex representation');
    let serialized: ByteArray = Felt252IntoHexAsciiByteArray::into(0x6F_felt252);
    assert_eq(@serialized, @expected_string, 'Bad felt252 hex representation');
}

#[test]
#[available_gas(10000000)]
fn test_to_string_dec() {
    let expected_string = "0";
    let serialized: ByteArray = U8IntoDecAsciiByteArray::into(0_u8);
    assert_eq(@serialized, @expected_string, 'Bad dec representation of 0');

    let expected_string = "10";
    let serialized: ByteArray = U8IntoDecAsciiByteArray::into(10_u8);
    assert_eq(@serialized, @expected_string, 'Bad dec representation of 10');

    let expected_string = "111";
    let serialized: ByteArray = U8IntoDecAsciiByteArray::into(111_u8);
    assert_eq(@serialized, @expected_string, 'Bad dec representation of 111');

    let expected_string = "255";
    let serialized: ByteArray = U8IntoDecAsciiByteArray::into(255_u8);
    assert_eq(@serialized, @expected_string, 'Bad dec representation of 255');

    // Other uint types:
    let expected_string = "111";
    let serialized: ByteArray = U16IntoDecAsciiByteArray::into(111_u16);
    assert_eq(@serialized, @expected_string, 'Bad u16 dec representation');
    let serialized: ByteArray = U32IntoDecAsciiByteArray::into(111_u32);
    assert_eq(@serialized, @expected_string, 'Bad u32 dec representation');
    let serialized: ByteArray = U64IntoDecAsciiByteArray::into(111_u64);
    assert_eq(@serialized, @expected_string, 'Bad u64 dec representation');
    let serialized: ByteArray = U128IntoDecAsciiByteArray::into(111_u128);
    assert_eq(@serialized, @expected_string, 'Bad u128 dec representation');
    let serialized: ByteArray = U256IntoDecAsciiByteArray::into(111_u256);
    assert_eq(@serialized, @expected_string, 'Bad u256 dec representation');
    let serialized: ByteArray = Felt252IntoDecAsciiByteArray::into(111_felt252);
    assert_eq(@serialized, @expected_string, 'Bad felt252 dec representation');
}

#[test]
#[available_gas(10000000)]
fn test_to_string_oct() {
    let expected_string = "0o0";
    let serialized: ByteArray = U8IntoOctAsciiByteArray::into(0_u8);
    assert_eq(@serialized, @expected_string, 'Bad oct representation of 0');

    let expected_string = "0o12";
    let serialized: ByteArray = U8IntoOctAsciiByteArray::into(10_u8);
    assert_eq(@serialized, @expected_string, 'Bad oct representation of 10');

    let expected_string = "0o157";
    let serialized: ByteArray = U8IntoOctAsciiByteArray::into(111_u8);
    assert_eq(@serialized, @expected_string, 'Bad oct representation of 111');

    let expected_string = "0o377";
    let serialized: ByteArray = U8IntoOctAsciiByteArray::into(255_u8);
    assert_eq(@serialized, @expected_string, 'Bad oct representation of 255');

    // Other uint types:
    let expected_string = "0o157";
    let serialized: ByteArray = U16IntoOctAsciiByteArray::into(111_u16);
    assert_eq(@serialized, @expected_string, 'Bad u16 oct representation');
    let serialized: ByteArray = U32IntoOctAsciiByteArray::into(111_u32);
    assert_eq(@serialized, @expected_string, 'Bad u32 oct representation');
    let serialized: ByteArray = U64IntoOctAsciiByteArray::into(111_u64);
    assert_eq(@serialized, @expected_string, 'Bad u64 oct representation');
    let serialized: ByteArray = U128IntoOctAsciiByteArray::into(111_u128);
    assert_eq(@serialized, @expected_string, 'Bad u128 oct representation');
    let serialized: ByteArray = U256IntoOctAsciiByteArray::into(111_u256);
    assert_eq(@serialized, @expected_string, 'Bad u256 oct representation');
    let serialized: ByteArray = Felt252IntoOctAsciiByteArray::into(111_felt252);
    assert_eq(@serialized, @expected_string, 'Bad felt252 oct representation');
}

#[test]
#[available_gas(10000000)]
fn test_to_string_bin() {
    let expected_string = "0b0";
    let serialized: ByteArray = U8IntoBinAsciiByteArray::into(0_u8);
    assert_eq(@serialized, @expected_string, 'Bad bin representation of 0');

    let expected_string = "0b1010";
    let serialized: ByteArray = U8IntoBinAsciiByteArray::into(10_u8);
    assert_eq(@serialized, @expected_string, 'Bad bin representation of 10');

    let expected_string = "0b1101111";
    let serialized: ByteArray = U8IntoBinAsciiByteArray::into(111_u8);
    assert_eq(@serialized, @expected_string, 'Bad bin representation of 111');

    let expected_string = "0b11111111";
    let serialized: ByteArray = U8IntoBinAsciiByteArray::into(255_u8);
    assert_eq(@serialized, @expected_string, 'Bad bin representation of 255');

    // Other uint types:
    let expected_string = "0b1101111";
    let serialized: ByteArray = U16IntoBinAsciiByteArray::into(111_u16);
    assert_eq(@serialized, @expected_string, 'Bad u16 bin representation');
    let serialized: ByteArray = U32IntoBinAsciiByteArray::into(111_u32);
    assert_eq(@serialized, @expected_string, 'Bad u32 bin representation');
    let serialized: ByteArray = U64IntoBinAsciiByteArray::into(111_u64);
    assert_eq(@serialized, @expected_string, 'Bad u64 bin representation');
    let serialized: ByteArray = U128IntoBinAsciiByteArray::into(111_u128);
    assert_eq(@serialized, @expected_string, 'Bad u128 bin representation');
    let serialized: ByteArray = U256IntoBinAsciiByteArray::into(111_u256);
    assert_eq(@serialized, @expected_string, 'Bad u256 bin representation');
    let serialized: ByteArray = Felt252IntoBinAsciiByteArray::into(111_felt252);
    assert_eq(@serialized, @expected_string, 'Bad felt252 bin representation');
}
