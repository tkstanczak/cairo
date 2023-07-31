use cairo_lang_defs::patcher::RewriteNode;
use cairo_lang_plugins::plugins::derive::{
    DeriveInfo, DeriveResult, MemberInfo, TraitDeriver, TypeVariantInfo,
};
use cairo_lang_syntax::node::SyntaxNode;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use indent::indent_by;
use indoc::indoc;
use itertools::intersperse;

struct StoreFns {
    read_body: RewriteNode,
    write_body: RewriteNode,
    read_at_offset_body: RewriteNode,
    write_at_offset_body: RewriteNode,
    size_body: RewriteNode,
}

/// Derive the `Store` trait for structs annotated with `derive(starknet::Store)`.
#[derive(Debug)]
pub struct StoreDeriver;
impl TraitDeriver for StoreDeriver {
    fn trait_name(&self) -> &str {
        "starknet::Store"
    }

    fn derive(&self, info: &DeriveInfo, derived: &SyntaxNode) -> DeriveResult {
        let fns = match &info.specific_info {
            TypeVariantInfo::Enum(variants) => enum_store_fns(info, variants),
            TypeVariantInfo::Struct(members) => struct_store_fns(info, members),
            TypeVariantInfo::Extern => return DeriveResult::unsupported_for_extern(derived),
        };
        DeriveResult::from_node(RewriteNode::interpolate_patched(
            indoc! {"
                $header$ {
                    fn read(
                        address_domain: u32, base: starknet::StorageBaseAddress
                    ) -> starknet::SyscallResult<$ty$> {
                        $read_body$
                    }
                    fn write(
                        address_domain: u32, base: starknet::StorageBaseAddress, value: $ty$
                    ) -> starknet::SyscallResult<()> {
                        $write_body$
                    }
                    fn read_at_offset(
                        address_domain: u32, base: starknet::StorageBaseAddress, offset: u8
                    ) -> starknet::SyscallResult<$ty$> {
                        $read_at_offset_body$
                    }
                    #[inline(always)]
                    fn write_at_offset(
                        address_domain: u32, base: starknet::StorageBaseAddress, offset: u8, value: $ty$
                    ) -> starknet::SyscallResult<()> {
                        $write_at_offset_body$
                    }
                    #[inline(always)]
                    fn size() -> u8 {
                        $size_body$
                    }
                }
            "},
            &[
                (
                    "header".into(),
                    info.impl_header(
                        derived,
                        "StarknetStore",
                        &[RewriteNode::from("starknet::Store"), RewriteNode::from("Destruct")],
                    ),
                ),
                ("ty".into(), info.full_typename()),
                ("read_body".into(), fns.read_body),
                ("write_body".into(), fns.write_body),
                ("read_at_offset_body".into(), fns.read_at_offset_body),
                ("write_at_offset_body".into(), fns.write_at_offset_body),
                ("size_body".into(), fns.size_body),
            ]
            .into(),
        ))
    }
}

/// Derive the `StorageAccess` trait for structs annotated with `derive(starknet::Store)`.
fn struct_store_fns(info: &DeriveInfo, members: &[MemberInfo]) -> StoreFns {
    let mut reads_values = vec![];
    let mut reads_values_at_offset = vec![];
    let mut reads_fields = vec![];
    let mut writes = vec![];
    let mut writes_at_offset = vec![];
    let mut sizes = vec![];

    for (i, field) in members.iter().enumerate() {
        let patches =
            [("field_name".into(), field.name.clone()), ("field_type".into(), field.ty.clone())]
                .into();
        if i == 0 {
            reads_values.push(RewriteNode::interpolate_patched(
                "let $field_name$ = starknet::Store::<$field_type$>::read(address_domain, base)?;",
                &patches,
            ));
            reads_values_at_offset.push(RewriteNode::interpolate_patched(
                "let $field_name$ = \
                 starknet::Store::<$field_type$>::read_at_offset(address_domain, base, offset)?;",
                &patches,
            ));
        } else {
            let subsequent_read = RewriteNode::interpolate_patched(
                "let $field_name$ = \
                 starknet::Store::<$field_type$>::read_at_offset(address_domain, base, \
                 current_offset)?;",
                &patches,
            );
            reads_values.push(subsequent_read.clone());
            reads_values_at_offset.push(subsequent_read);
        }
        if i < members.len() - 1 {
            if i == 0 {
                reads_values.push(RewriteNode::interpolate_patched(
                    "let mut current_offset = starknet::Store::<$field_type$>::size();",
                    &patches,
                ));
                reads_values_at_offset.push(RewriteNode::interpolate_patched(
                    "let mut current_offset = offset + starknet::Store::<$field_type$>::size();",
                    &patches,
                ));
            } else {
                let subsequent_read = RewriteNode::interpolate_patched(
                    "current_offset += starknet::Store::<$field_type$>::size();",
                    &patches,
                );
                reads_values.push(subsequent_read.clone());
                reads_values_at_offset.push(subsequent_read);
            }
        }

        reads_fields.push(RewriteNode::interpolate_patched("$field_name$,", &patches));

        if i == 0 {
            writes.push(RewriteNode::interpolate_patched(
                "starknet::Store::<$field_type$>::write(address_domain, base, \
                 value.$field_name$)?;",
                &patches,
            ));
            writes_at_offset.push(RewriteNode::interpolate_patched(
                "starknet::Store::<$field_type$>::write_at_offset(address_domain, base, offset, \
                 value.$field_name$)?;",
                &patches,
            ));
        } else {
            let subsequent_write = RewriteNode::interpolate_patched(
                "starknet::Store::<$field_type$>::write_at_offset(address_domain, base, \
                 current_offset, value.$field_name$)?;",
                &patches,
            );
            writes.push(subsequent_write.clone());
            writes_at_offset.push(subsequent_write);
        }

        if i < members.len() - 1 {
            if i == 0 {
                writes.push(RewriteNode::interpolate_patched(
                    "let mut current_offset = starknet::Store::<$field_type$>::size();",
                    &patches,
                ));
                writes_at_offset.push(RewriteNode::interpolate_patched(
                    "let mut current_offset = offset + starknet::Store::<$field_type$>::size();",
                    &patches,
                ));
            } else {
                let subsequent_write = RewriteNode::interpolate_patched(
                    "current_offset += starknet::Store::<$field_type$>::size();",
                    &patches,
                );
                writes.push(subsequent_write.clone());
                writes_at_offset.push(subsequent_write);
            }
        }
        sizes.push(RewriteNode::interpolate_patched(
            "starknet::Store::<$field_type$>::size()",
            &patches,
        ));
    }
    let reads_values = RewriteNode::new_modified(
        intersperse(reads_values.into_iter(), RewriteNode::from("\n        ")).collect(),
    );
    let reads_fields = RewriteNode::new_modified(
        intersperse(reads_fields.into_iter(), RewriteNode::from("\n                ")).collect(),
    );
    let writes = RewriteNode::new_modified(
        intersperse(writes.into_iter(), RewriteNode::from("\n        ")).collect(),
    );
    let reads_values_at_offset = RewriteNode::new_modified(
        intersperse(reads_values_at_offset.into_iter(), RewriteNode::from("\n        ")).collect(),
    );
    let writes_at_offset = RewriteNode::new_modified(
        intersperse(writes_at_offset.into_iter(), RewriteNode::from("\n        ")).collect(),
    );
    let sizes = RewriteNode::new_modified(
        intersperse(sizes.into_iter(), RewriteNode::from(" +\n        ")).collect(),
    );
    StoreFns {
        read_body: RewriteNode::interpolate_patched(
            indent_by(
                8,
                indoc! {"
                $reads_values$
                starknet::SyscallResult::Ok(
                    $struct_name$ {
                        $reads_fields$
                    }
                )"},
            )
            .as_str(),
            &[
                ("reads_values".into(), reads_values),
                ("struct_name".into(), info.name.clone()),
                ("reads_fields".into(), reads_fields.clone()),
            ]
            .into(),
        ),
        write_body: RewriteNode::interpolate_patched(
            indent_by(
                8,
                indoc! {"
                $writes$
                starknet::SyscallResult::Ok(())"},
            )
            .as_str(),
            &[("writes".into(), writes)].into(),
        ),
        read_at_offset_body: RewriteNode::interpolate_patched(
            indent_by(
                8,
                indoc! {"
                $reads_values_at_offset$
                starknet::SyscallResult::Ok(
                    $struct_name$ {
                        $reads_fields$
                    }
                )"},
            )
            .as_str(),
            &[
                ("reads_values_at_offset".into(), reads_values_at_offset),
                ("struct_name".into(), info.name.clone()),
                ("reads_fields".into(), reads_fields),
            ]
            .into(),
        ),
        write_at_offset_body: RewriteNode::interpolate_patched(
            indent_by(
                8,
                indoc! {"
                $writes_at_offset$
                starknet::SyscallResult::Ok(())"},
            )
            .as_str(),
            &[("writes_at_offset".into(), writes_at_offset)].into(),
        ),
        size_body: RewriteNode::interpolate_patched("$sizes$", &[("sizes".into(), sizes)].into()),
    }
}

/// Derive the `StorageAccess` trait for enums annotated with `derive(starknet::Store)`.
fn enum_store_fns(info: &DeriveInfo, variants: &[MemberInfo]) -> StoreFns {
    let mut match_idx = vec![];
    let mut match_idx_at_offset = vec![];

    let mut match_value = vec![];
    let mut match_value_at_offset = vec![];

    let mut patches: UnorderedHashMap<_, _> = [("enum_name".into(), info.name.clone())].into();
    for (i, variant) in variants.iter().enumerate() {
        patches.insert("variant_name".into(), variant.name.clone());
        patches.insert("variant_type".into(), variant.ty.clone());
        patches.insert("idx".into(), RewriteNode::Text(i.to_string()));
        match_idx.push(RewriteNode::interpolate_patched(
            indoc! {"
            if idx == $idx$ {
                        starknet::SyscallResult::Ok($enum_name$::$variant_name$(
                            starknet::Store::read_at_offset(address_domain, base, 1_u8)?
                        ))
                    }"},
            &patches,
        ));
        match_idx_at_offset.push(RewriteNode::interpolate_patched(
            indoc! {"
            if idx == $idx$ {
                        starknet::SyscallResult::Ok($enum_name$::$variant_name$(
                            starknet::Store::read_at_offset(address_domain, base, offset + 1_u8)?
                        ))
                    }"},
            &patches,
        ));
        match_value.push(RewriteNode::interpolate_patched(
            "$enum_name$::$variant_name$(x) => {
                starknet::Store::write(address_domain, base, $idx$)?;
                starknet::Store::write_at_offset(address_domain, base, 1_u8, x)?;
            },",
            &patches,
        ));
        match_value_at_offset.push(RewriteNode::interpolate_patched(
            "$enum_name$::$variant_name$(x) => {
                starknet::Store::write_at_offset(address_domain, base, offset, $idx$)?;
                starknet::Store::write_at_offset(address_domain, base, offset + 1_u8, x)?;
            },",
            &patches,
        ));

        let match_size = if patches.contains_key("match_size") {
            RewriteNode::interpolate_patched(
                "cmp::max(starknet::Store::<$variant_type$>::size(), $match_size$)",
                &patches,
            )
        } else {
            RewriteNode::interpolate_patched("starknet::Store::<$variant_type$>::size()", &patches)
        };
        patches.insert("match_size".into(), match_size);
    }
    patches.insert(
        "match_idx".into(),
        RewriteNode::new_modified(
            intersperse(match_idx.into_iter(), RewriteNode::from("\n        else ")).collect(),
        ),
    );
    patches.insert(
        "match_idx_at_offset".into(),
        RewriteNode::new_modified(
            intersperse(match_idx_at_offset.into_iter(), RewriteNode::from("\n        else "))
                .collect(),
        ),
    );
    patches.insert(
        "match_value".into(),
        RewriteNode::new_modified(
            intersperse(match_value.into_iter(), RewriteNode::from("\n            ")).collect(),
        ),
    );
    patches.insert(
        "match_value_at_offset".into(),
        RewriteNode::new_modified(
            intersperse(match_value_at_offset.into_iter(), RewriteNode::from("\n            "))
                .collect(),
        ),
    );
    StoreFns {
        read_body: RewriteNode::interpolate_patched(
            indent_by(
                8,
                indoc! {"
                let idx = starknet::Store::<felt252>::read(address_domain, base)?;
                $match_idx$
                else {
                    let mut message = Default::default();
                    message.append('Incorrect index:');
                    message.append(idx);
                    starknet::SyscallResult::Err(message)
                }"},
            )
            .as_str(),
            &patches,
        ),
        write_body: RewriteNode::interpolate_patched(
            indent_by(
                8,
                indoc! {"
                match value {
                    $match_value$
                }
                starknet::SyscallResult::Ok(())"},
            )
            .as_str(),
            &patches,
        ),
        read_at_offset_body: RewriteNode::interpolate_patched(
            indent_by(
                8,
                indoc! {"
                let idx = starknet::Store::<felt252>::read_at_offset(address_domain, base, offset)?;
                $match_idx_at_offset$
                else {
                    let mut message = Default::default();
                    message.append('Incorrect index:');
                    message.append(idx);
                    starknet::SyscallResult::Err(message)
                }"},
            )
            .as_str(),
            &patches,
        ),
        write_at_offset_body: RewriteNode::interpolate_patched(
            indent_by(
                8,
                indoc! {"
            match value {
                $match_value_at_offset$
            }
            starknet::SyscallResult::Ok(())"},
            )
            .as_str(),
            &patches,
        ),
        size_body: RewriteNode::interpolate_patched("1 + $match_size$", &patches),
    }
}
