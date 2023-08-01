use cairo_lang_compiler::diagnostics;
use cairo_lang_defs::patcher::{ModifiedNode, PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    DynGeneratedFileAuxData, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_plugins::plugins::derive::{
    DeriveInfo, DeriveResult, MemberInfo, TraitDeriver, TypeVariantInfo,
};
use cairo_lang_syntax::attribute::structured::{
    self, AttributeArg, AttributeArgVariant, AttributeStructurize,
};
use cairo_lang_syntax::node::ast::{self, OptionWrappedGenericParamList};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{SyntaxNode, Terminal, TypedSyntaxNode};
use indoc::indoc;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;

use super::aux_data::StarkNetEventAuxData;
use crate::contract::starknet_keccak;

/// Generated auxiliary data for the `#[derive(starknet::Event)]` attribute.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum EventData {
    Struct { members: Vec<(SmolStr, EventFieldKind)> },
    Enum { variants: Vec<(SmolStr, EventFieldKind)> },
}

/// Describes how to serialize the event's field.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum EventFieldKind {
    // Serialize to `keys` using `Serde`.
    #[serde(rename = "key")]
    KeySerde,
    // Serialize to `data` using `Serde`.
    #[serde(rename = "data")]
    DataSerde,
    // Serialize as a nested event.
    #[serde(rename = "nested")]
    Nested,
}

struct EventCode {
    append_keys_and_data_body: RewriteNode,
    deserialize_body: RewriteNode,
    additional_impls: RewriteNode,
}

/// Derive the `Store` trait for structs annotated with `derive(starknet::Store)`.
#[derive(Debug)]
pub struct EventDeriver;
impl TraitDeriver for EventDeriver {
    fn trait_name(&self) -> &str {
        "starknet::Event"
    }

    fn aux_data(&self, db: &dyn SyntaxGroup, info: &DeriveInfo) -> Option<DynGeneratedFileAuxData> {
        Some(DynGeneratedFileAuxData::new(StarkNetEventAuxData {
            event_data: match &info.specific_info {
                TypeVariantInfo::Enum(variants) => EventData::Enum {
                    variants: variants
                        .iter()
                        .map(|variant| {
                            (
                                variant.name.get_text(db).into(),
                                get_field_kind_for_member(
                                    &mut vec![],
                                    &variant.attributes,
                                    EventFieldKind::Nested,
                                ),
                            )
                        })
                        .collect(),
                },
                TypeVariantInfo::Struct(members) => EventData::Struct {
                    members: members
                        .iter()
                        .map(|member| {
                            (
                                member.name.get_text(db).into(),
                                get_field_kind_for_member(
                                    &mut vec![],
                                    &member.attributes,
                                    EventFieldKind::DataSerde,
                                ),
                            )
                        })
                        .collect(),
                },
                TypeVariantInfo::Extern => return None,
            },
        }))
    }

    fn derive(&self, info: &DeriveInfo, derived: &SyntaxNode) -> DeriveResult {
        let (event_info, diagnostics) = match &info.specific_info {
            TypeVariantInfo::Enum(variants) => enum_event_info(info, variants),
            TypeVariantInfo::Struct(members) => struct_event_info(info, members),
            TypeVariantInfo::Extern => return DeriveResult::unsupported_for_extern(derived),
        };
        let code = RewriteNode::interpolate_patched(
            indoc! {"
                $header$ {
                    fn append_keys_and_data(
                        self: @$ty$, ref keys: Array<felt252>, ref data: Array<felt252>
                    ) {$append_keys_and_data_body$}
                    fn deserialize(
                        ref keys: Span<felt252>, ref data: Span<felt252>,
                    ) -> Option<$ty$> {
                        $deserialize_body$
                    }
                }
                $additional_impls$
            "},
            &[
                (
                    "header".into(),
                    info.impl_header(
                        derived,
                        "StarknetEvent",
                        &[RewriteNode::from("starknet::Event"), RewriteNode::from("Destruct")],
                    ),
                ),
                ("ty".into(), info.full_typename()),
                ("append_keys_and_data_body".into(), event_info.append_keys_and_data_body),
                ("deserialize_body".into(), event_info.deserialize_body),
                ("additional_impls".into(), event_info.additional_impls),
            ]
            .into(),
        );
        DeriveResult { code: Some(code), diagnostics }
    }
}

/// Derive the `Event` trait for structs annotated with `derive(starknet::Event)`.
fn struct_event_info(
    info: &DeriveInfo,
    members: &[MemberInfo],
) -> (EventCode, Vec<PluginDiagnostic>) {
    let mut diagnostics = vec![];

    // Generate append_keys_and_data() code.
    let mut append_members = vec![];
    let mut deserialize_members = vec![];
    let mut ctor = vec![];
    for member in members.iter() {
        let member_kind = get_field_kind_for_member(
            &mut diagnostics,
            &member.attributes,
            EventFieldKind::DataSerde,
        );

        let member_for_append = RewriteNode::interpolate_patched(
            "self.$member_name$",
            &[(String::from("member_name"), member.name.clone())].into(),
        );
        let append_member = append_field(member_kind, member_for_append);
        let deserialize_member = deserialize_field(member_kind, member.name.clone());
        append_members.push(append_member);
        deserialize_members.push(deserialize_member);
        ctor.push(RewriteNode::interpolate_patched(
            "$member_name$, ",
            &[(String::from("member_name"), member.name.clone())].into(),
        ));
    }
    let append_members = RewriteNode::new_modified(append_members);
    let deserialize_members = RewriteNode::new_modified(deserialize_members);
    let ctor = RewriteNode::new_modified(ctor);

    // Add an implementation for `Event<StructName>`.
    let event_impl = RewriteNode::interpolate_patched(
        indoc! {"
            impl $struct_name$IsEvent of starknet::Event<$struct_name$> {
                fn append_keys_and_data(
                    self: @$struct_name$, ref keys: Array<felt252>, ref data: Array<felt252>
                ) {$append_members$
                }
                fn deserialize(
                    ref keys: Span<felt252>, ref data: Span<felt252>,
                ) -> Option<$struct_name$> {$deserialize_members$
                    Option::Some($struct_name$ {$ctor$})
                }
            }"},
        &[
            (String::from("struct_name"), struct_name),
            (String::from("append_members"), append_members),
            (String::from("deserialize_members"), deserialize_members),
            (String::from("ctor"), ctor),
        ]
        .into(),
    );
<<<<<<< HEAD

    builder.add_modified(event_impl);

    PluginResult {
        code: Some(PluginGeneratedFile {
            name: "event_impl".into(),
            content: builder.code,
            patches: builder.patches,
            aux_data: Some(DynGeneratedFileAuxData::new(StarkNetEventAuxData { event_data })),
        }),
||||||| parent of ea008485f... Using derivers for event trait.

    builder.add_modified(event_impl);

    PluginResult {
        code: Some(PluginGeneratedFile {
            name: "event_impl".into(),
            content: builder.code,
            patches: builder.patches,
            aux_data: vec![DynGeneratedFileAuxData::new(StarkNetEventAuxData { event_data })],
        }),
=======
    (
        EventCode {
            append_keys_and_data_body: append_members,
            deserialize_body: RewriteNode::interpolate_patched(
                indoc! {"
                $deserialize_members$ 
                        Option::Some($struct_name$ {$ctor$})"},
                &[
                    (String::from("append_members"), append_members),
                    (String::from("deserialize_members"), deserialize_members),
                    (String::from("ctor"), ctor),
                ]
                .into(),
            ),
            additional_impls: RewriteNode::Text("".to_string()),
        },
>>>>>>> ea008485f... Using derivers for event trait.
        diagnostics,
    )
}

/// Retrieves the field kind for a given struct member or enum variant,
/// indicating how the field should be serialized.
/// See [EventFieldKind].
fn get_field_kind_for_member(
    diagnostics: &mut Vec<PluginDiagnostic>,
    attrs: &[structured::Attribute],
    default: EventFieldKind,
) -> EventFieldKind {
    let nested = attrs.iter().find(|attr| attr.id == "nested");
    let is_key = attrs.iter().any(|attr| attr.id == "key");
    let serde = attrs.iter().find(|attr| attr.id == "serde");

    // Currently, nested fields are unsupported.
    if let Some(nested) = nested {
        diagnostics.push(PluginDiagnostic {
            message: "Nested event fields are currently unsupported".to_string(),
            stable_ptr: nested.stable_ptr.untyped(),
        });
    }
    // Currently, serde fields are unsupported.
    if let Some(serde) = serde {
        diagnostics.push(PluginDiagnostic {
            message: "Serde event fields are currently unsupported".to_string(),
            stable_ptr: serde.stable_ptr.untyped(),
        });
    }

    if is_key {
        return EventFieldKind::KeySerde;
    }
    default
}

/// Derive the `Event` trait for enums annotated with `derive(starknet::Event)`.
fn enum_event_info(
    info: &DeriveInfo,
    variants: &[MemberInfo],
) -> (EventCode, Vec<PluginDiagnostic>) {
    let mut diagnostics = vec![];

    let mut append_variants = vec![];
    let mut deserialize_variants = vec![];
    let mut event_into_impls = vec![];
    for variant in variants {
        let variant_selector =
            format!("0x{:x}", starknet_keccak(variant.name.get_text(db).as_bytes()));
        let member_kind = get_field_kind_for_member(
            &mut diagnostics,
            &variant.attributes,
            EventFieldKind::Nested,
        );
        let append_member = append_field(member_kind, RewriteNode::Text("val".into()));
        let append_variant = RewriteNode::interpolate_patched(
            "
            $enum_name$::$variant_name$(val) => {
                array::ArrayTrait::append(ref keys, $variant_selector$);$append_member$
            },",
            &[
                (String::from("enum_name"), enum_name.clone()),
                (String::from("variant_name"), variant_name.clone()),
                (String::from("variant_selector"), RewriteNode::Text(variant_selector.clone())),
                (String::from("append_member"), append_member),
            ]
            .into(),
        );
        let deserialize_member = deserialize_field(member_kind, RewriteNode::Text("val".into()));
        let deserialize_variant = RewriteNode::interpolate_patched(
            "
            if selector == $variant_selector$ {$deserialize_member$
                return Option::Some($enum_name$::$variant_name$(val));
            }",
            &[
                (String::from("enum_name"), enum_name.clone()),
                (String::from("variant_name"), variant_name.clone()),
                (String::from("variant_selector"), RewriteNode::Text(variant_selector)),
                (String::from("deserialize_member"), deserialize_member),
            ]
            .into(),
        );
        append_variants.push(append_variant);
        deserialize_variants.push(deserialize_variant);
        let into_impl = RewriteNode::interpolate_patched(
            indoc! {"
                impl $enum_name$$variant_name$IntoEvent of Into<$ty$, $enum_name$> {
                    fn into(self: $ty$) -> $enum_name$ {
                        $enum_name$::$variant_name$(self)
                    }
                }
                "},
            &[
                (String::from("enum_name"), enum_name.clone()),
                (String::from("variant_name"), variant_name),
                (String::from("ty"), ty),
            ]
            .into(),
        );
        event_into_impls.push(into_impl);
    }
    let event_data = EventData::Enum { variants };
    let append_variants = RewriteNode::Modified(ModifiedNode { children: Some(append_variants) });
    let deserialize_variants =
        RewriteNode::Modified(ModifiedNode { children: Some(deserialize_variants) });

    // Add an implementation for `Event<StructName>`.
    let event_impl = RewriteNode::interpolate_patched(
        indoc! {"
            impl $enum_name$IsEvent of starknet::Event<$enum_name$> {
                fn append_keys_and_data(
                    self: @$enum_name$, ref keys: Array<felt252>, ref data: Array<felt252>
                ) {
                    match self {$append_variants$
                    }
                }
                fn deserialize(
                    ref keys: Span<felt252>, ref data: Span<felt252>,
                ) -> Option<$enum_name$> {
                    let selector = *array::SpanTrait::pop_front(ref keys)?;
                    $deserialize_variants$
                    Option::None
                }
            }
            $event_into_impls$
        "},
        &[
            (String::from("enum_name"), enum_name),
            (String::from("append_variants"), append_variants),
            (String::from("deserialize_variants"), deserialize_variants),
            (
                String::from("event_into_impls"),
                RewriteNode::Modified(ModifiedNode { children: Some(event_into_impls) }),
            ),
        ]
        .into(),
    );

    builder.add_modified(event_impl);
<<<<<<< HEAD

    PluginResult {
        code: Some(PluginGeneratedFile {
            name: "event_impl".into(),
            content: builder.code,
            patches: builder.patches,
            aux_data: Some(DynGeneratedFileAuxData::new(StarkNetEventAuxData { event_data })),
        }),
||||||| parent of ea008485f... Using derivers for event trait.

    PluginResult {
        code: Some(PluginGeneratedFile {
            name: "event_impl".into(),
            content: builder.code,
            patches: builder.patches,
            aux_data: vec![DynGeneratedFileAuxData::new(StarkNetEventAuxData { event_data })],
        }),
=======
    (
        EventCode {
            append_keys_and_data_body: todo!(),
            deserialize_body: todo!(),
            additional_impls: todo!(),
        },
>>>>>>> ea008485f... Using derivers for event trait.
        diagnostics,
    )
}

/// Returns true if the type should be derived as an event.
pub fn derive_event_needed<T: QueryAttrs>(with_attrs: &T, db: &dyn SyntaxGroup) -> bool {
    with_attrs.query_attr(db, "derive").into_iter().any(|attr| {
        let attr = attr.structurize(db);
        for arg in &attr.args {
            let AttributeArg {
                variant: AttributeArgVariant::Unnamed { value: ast::Expr::Path(path), .. },
                ..
            } = arg
            else {
                continue;
            };
            if path.as_syntax_node().get_text_without_trivia(db) == "starknet::Event" {
                return true;
            }
        }
        false
    })
}

/// Generates code to emit an event for a field
fn append_field(member_kind: EventFieldKind, field: RewriteNode) -> RewriteNode {
    match member_kind {
        EventFieldKind::Nested => RewriteNode::interpolate_patched(
            "
                starknet::Event::append_keys_and_data(
                    $field$, ref keys, ref data
                );",
            &[(String::from("field"), field)].into(),
        ),
        EventFieldKind::KeySerde => RewriteNode::interpolate_patched(
            "
                serde::Serde::serialize($field$, ref keys);",
            &[(String::from("field"), field)].into(),
        ),
        EventFieldKind::DataSerde => RewriteNode::interpolate_patched(
            "
                serde::Serde::serialize($field$, ref data);",
            &[(String::from("field"), field)].into(),
        ),
    }
}

fn deserialize_field(member_kind: EventFieldKind, member_name: RewriteNode) -> RewriteNode {
    match member_kind {
        EventFieldKind::Nested => RewriteNode::interpolate_patched(
            "
                let $member_name$ = starknet::Event::deserialize(
                    ref keys, ref data
                )?;",
            &[(String::from("member_name"), member_name)].into(),
        ),
        EventFieldKind::KeySerde => RewriteNode::interpolate_patched(
            "
                let $member_name$ = serde::Serde::deserialize(
                    ref keys
                )?;",
            &[(String::from("member_name"), member_name)].into(),
        ),
        EventFieldKind::DataSerde => RewriteNode::interpolate_patched(
            "
                let $member_name$ = serde::Serde::deserialize(
                    ref data
                )?;",
            &[(String::from("member_name"), member_name)].into(),
        ),
    }
}
