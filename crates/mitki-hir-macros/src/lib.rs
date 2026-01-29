use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::parse::{Parse, ParseStream};
use syn::{Ident, Result, Token, braced, parenthesized};

#[proc_macro]
pub fn define_hir(input: TokenStream) -> TokenStream {
    let schema = syn::parse_macro_input!(input as Schema);
    schema.expand().into()
}

struct Schema {
    categories: Vec<Ident>,
    nodes: Vec<NodeDef>,
}

impl Parse for Schema {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let categories_ident: Ident = input.parse()?;
        if categories_ident != "categories" {
            return Err(input.error("expected `categories`"));
        }

        let categories_content;
        braced!(categories_content in input);
        let mut categories = Vec::new();
        while !categories_content.is_empty() {
            let ident: Ident = categories_content.parse()?;
            categories.push(ident);
            if categories_content.peek(Token![,]) {
                let _comma: Token![,] = categories_content.parse()?;
            }
        }

        let nodes_ident: Ident = input.parse()?;
        if nodes_ident != "nodes" {
            return Err(input.error("expected `nodes`"));
        }
        let nodes_content;
        braced!(nodes_content in input);
        let mut nodes = Vec::new();
        while !nodes_content.is_empty() {
            nodes.push(nodes_content.parse()?);
            if nodes_content.peek(Token![;]) {
                let _semi: Token![;] = nodes_content.parse()?;
            }
        }

        Ok(Self { categories, nodes })
    }
}

struct NodeDef {
    name: Ident,
    layout: Ident,
    fields: Vec<FieldDef>,
    category: Option<Ident>,
}

impl Parse for NodeDef {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let name: Ident = input.parse()?;
        let _colon: Token![:] = input.parse()?;
        let layout: Ident = input.parse()?;

        let mut fields = Vec::new();
        if input.peek(syn::token::Paren) {
            let fields_content;
            parenthesized!(fields_content in input);
            while !fields_content.is_empty() {
                fields.push(fields_content.parse()?);
                if fields_content.peek(Token![,]) {
                    let _comma: Token![,] = fields_content.parse()?;
                }
            }
        }

        let _fat_arrow: Token![=>] = input.parse()?;
        let category = if input.peek(Token![_]) {
            let _underscore: Token![_] = input.parse()?;
            None
        } else {
            Some(input.parse()?)
        };

        Ok(Self { name, layout, fields, category })
    }
}

struct FieldDef {
    name: Ident,
    ty: TypeSpec,
    arg_ty: Option<TypeSpec>,
}

impl Parse for FieldDef {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let name: Ident = input.parse()?;
        let _colon: Token![:] = input.parse()?;
        let ty: TypeSpec = input.parse()?;

        let arg_ty = if input.peek(Token![<]) && input.peek2(Token![-]) {
            let _lt: Token![<] = input.parse()?;
            let _dash: Token![-] = input.parse()?;
            let arg: TypeSpec = input.parse()?;
            Some(arg)
        } else {
            None
        };

        Ok(Self { name, ty, arg_ty })
    }
}

#[derive(Clone, Debug)]
enum TypeSpec {
    Symbol,
    OptionSymbol,
    Node(Ident),
}

impl TypeSpec {
    fn parse_option(input: ParseStream<'_>) -> Result<Self> {
        let _lt: Token![<] = input.parse()?;
        let inner: Ident = input.parse()?;
        let _gt: Token![>] = input.parse()?;
        if inner == "Symbol" {
            Ok(TypeSpec::OptionSymbol)
        } else {
            Err(input.error("Option<> only supports Symbol"))
        }
    }
}

impl Parse for TypeSpec {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let ident: Ident = input.parse()?;
        if ident == "Symbol" {
            return Ok(TypeSpec::Symbol);
        }
        if ident == "Option" {
            if input.peek(Token![<]) {
                return TypeSpec::parse_option(input);
            }
            return Err(input.error("Option requires type arguments"));
        }
        Ok(TypeSpec::Node(ident))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum LayoutKind {
    BindingInLhs,
    ListRange,
    CallRange,
    BlockWithTail,
    TripleLane,
    Direct2,
    ZeroZero,
}

impl LayoutKind {
    fn parse(ident: &Ident) -> Result<Self> {
        match ident.to_string().as_str() {
            "BindingInLhs" => Ok(LayoutKind::BindingInLhs),
            "ListRange" => Ok(LayoutKind::ListRange),
            "CallRange" => Ok(LayoutKind::CallRange),
            "BlockWithTail" => Ok(LayoutKind::BlockWithTail),
            "TripleLane" => Ok(LayoutKind::TripleLane),
            "Direct2" => Ok(LayoutKind::Direct2),
            "ZeroZero" => Ok(LayoutKind::ZeroZero),
            _ => Err(syn::Error::new_spanned(ident, "unknown layout")),
        }
    }
}

impl Schema {
    fn expand(&self) -> proc_macro2::TokenStream {
        let categories = &self.categories;
        let node_names: Vec<_> = self.nodes.iter().map(|node| node.name.clone()).collect();

        let mut layout_nodes = Vec::new();
        for node in &self.nodes {
            let layout = LayoutKind::parse(&node.layout).unwrap();
            layout_nodes.push((node, layout));
        }

        let node_kind_variants = node_names.iter();

        let tag_defs = {
            let mut defs = Vec::new();
            defs.push(quote! { #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, salsa::Update)] pub(crate) struct HirTag; });
            defs.push(quote! { pub(crate) type HirId = crate::hir::id::Id<HirTag>; });
            defs.push(quote! { #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, salsa::Update)] pub(crate) struct SymTag; });
            defs.push(quote! { pub(crate) type SymId = crate::hir::id::Id<SymTag>; });
            defs.push(quote! { #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, salsa::Update)] pub(crate) struct IxTag; });
            defs.push(quote! { pub(crate) type IxId = crate::hir::id::Id<IxTag>; });
            for cat in categories {
                let tag = format_ident!("{}Tag", cat);
                let id = format_ident!("{}Id", cat);
                defs.push(quote! { #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, salsa::Update)] pub struct #tag; });
                defs.push(quote! { pub type #id = crate::hir::id::Id<#tag>; });
            }
            for node in &node_names {
                let tag = format_ident!("{}Tag", node);
                let id = format_ident!("{}Id", node);
                defs.push(quote! { #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, salsa::Update)] pub struct #tag; });
                defs.push(quote! { pub type #id = crate::hir::id::Id<#tag>; });
            }
            quote! { #(#defs)* }
        };

        let mut node_id_trait_impls = Vec::new();
        let mut node_id_trait_impls_categories = Vec::new();

        for node in &node_names {
            let id = format_ident!("{}Id", node);
            node_id_trait_impls.push(quote! {
                impl sealed::Sealed for #id {}
                impl NodeId for #id {
                    fn raw(self) -> u32 {
                        self.raw().0
                    }
                }
            });
        }
        for cat in categories {
            let id = format_ident!("{}Id", cat);
            node_id_trait_impls_categories.push(quote! {
                impl sealed::Sealed for #id {}
                impl NodeId for #id {
                    fn raw(self) -> u32 {
                        self.raw().0
                    }
                }
            });
        }

        let mut list_wrappers = Vec::new();
        let mut list_wrapper_names = Vec::new();
        let mut list_types = Vec::new();

        for (node, layout) in &layout_nodes {
            match layout {
                LayoutKind::ListRange => {
                    if let Some(field) = node.fields.first() {
                        list_types.push(field.ty.clone());
                    }
                }
                LayoutKind::CallRange => {
                    if node.fields.len() > 1 {
                        list_types.push(node.fields[1].ty.clone());
                    }
                }
                LayoutKind::BlockWithTail => {
                    if let Some(field) = node.fields.first() {
                        list_types.push(field.ty.clone());
                    }
                }
                _ => {}
            }
        }

        list_types.sort_by_key(format_type);
        list_types.dedup_by(|a, b| format_type(a) == format_type(b));

        for ty in &list_types {
            if let Some((wrapper, id_ty)) = list_wrapper_for(ty) {
                if list_wrapper_names.iter().any(|it| it == &wrapper) {
                    continue;
                }
                let wrapper_name = wrapper.clone();
                let id_ty = id_ty.clone();
                let iter_ty = quote! { impl Iterator<Item = #id_ty> + '_ };
                list_wrappers.push(quote! {
                    pub struct #wrapper_name<'a> {
                        slice: &'a [u32],
                    }

                    impl<'a> #wrapper_name<'a> {
                        pub fn len(&self) -> usize { self.slice.len() }
                        pub fn is_empty(&self) -> bool { self.slice.is_empty() }
                        pub fn iter(&self) -> #iter_ty {
                            self.slice
                                .iter()
                                .copied()
                                .map(|raw| #id_ty::from_raw(crate::hir::id::Raw(raw)))
                        }
                        pub fn get(&self, index: usize) -> Option<#id_ty> {
                            self.slice
                                .get(index)
                                .copied()
                                .map(|raw| #id_ty::from_raw(crate::hir::id::Raw(raw)))
                        }
                    }

                    impl<'a> IntoIterator for #wrapper_name<'a> {
                        type Item = #id_ty;
                        type IntoIter = std::iter::Map<
                            std::iter::Copied<std::slice::Iter<'a, u32>>,
                            fn(u32) -> #id_ty,
                        >;
                        fn into_iter(self) -> Self::IntoIter {
                            fn map_fn(raw: u32) -> #id_ty {
                                #id_ty::from_raw(crate::hir::id::Raw(raw))
                            }
                            self.slice.iter().copied().map(map_fn)
                        }
                    }
                });
                list_wrapper_names.push(wrapper_name);
            }
        }

        let mut view_structs = Vec::new();
        for (node, layout) in &layout_nodes {
            if matches!(layout, LayoutKind::TripleLane | LayoutKind::Direct2)
                && let Some(struct_name) = view_struct_name(&node.name)
            {
                let fields = node.fields.iter().map(|field| {
                    let field_name = &field.name;
                    let field_ty = type_to_id(&field.ty);
                    quote! { pub #field_name: #field_ty }
                });
                view_structs.push(quote! {
                    pub struct #struct_name {
                        #(#fields,)*
                    }
                });
            }
        }

        let mut constructors = Vec::new();
        let mut accessors = Vec::new();
        let mut downcasts = Vec::new();
        let mut upcasts = Vec::new();

        for (node, layout) in &layout_nodes {
            let node_name = &node.name;
            let node_id = format_ident!("{}Id", node_name);
            let kind_variant = node_name;

            if let Some(category) = &node.category {
                let cat_id = format_ident!("{}Id", category);
                upcasts.push(quote! {
                    impl From<#node_id> for #cat_id {
                        fn from(value: #node_id) -> #cat_id {
                            #cat_id::from_raw(value.raw())
                        }
                    }
                });
            }

            let downcast_name = format_ident!("as_{}", to_snake(&node_name.to_string()));
            downcasts.push(quote! {
                pub fn #downcast_name(&self, node: impl NodeId) -> Option<#node_id> {
                    let hir = HirId::from_raw(crate::hir::id::Raw(node.raw()));
                    if self.node_kind_raw(hir) == NodeKind::#kind_variant {
                        Some(#node_id::from_raw(hir.raw()))
                    } else {
                        None
                    }
                }
            });

            let ctor_name = format_ident!("alloc_{}", constructor_suffix(node_name));
            let accessor_name = format_ident!("{}", accessor_name(node_name));

            let (ctor_sig, ctor_body, accessor_sig, accessor_body) = match layout {
                LayoutKind::BindingInLhs => {
                    let field = node.fields.first().expect("BindingInLhs requires a field");
                    let arg_ty = field.arg_ty.clone().unwrap_or_else(|| field.ty.clone());
                    let arg_ty_tokens = type_to_arg(&arg_ty);
                    let arg_name = &field.name;
                    let lhs_expr = match &arg_ty {
                        TypeSpec::Symbol => {
                            quote! { self.intern_symbol(#arg_name).raw() }
                        }
                        TypeSpec::OptionSymbol => {
                            quote! {
                                #arg_name.map_or(crate::hir::id::Raw::ZERO, |sym| self.intern_symbol(sym).raw())
                            }
                        }
                        _ => panic!("BindingInLhs only supports Symbol types"),
                    };
                    let ctor_sig = quote! { pub fn #ctor_name(&mut self, #arg_name: #arg_ty_tokens) -> #node_id };
                    let ctor_body = quote! {
                        let lhs = #lhs_expr;
                        let hir = self.push_node(NodeKind::#kind_variant, lhs, crate::hir::id::Raw::ZERO);
                        #node_id::from_raw(hir.raw())
                    };
                    let accessor_sig =
                        quote! { pub fn #accessor_name(&self, node: #node_id) -> #arg_ty_tokens };
                    let accessor_body = match &field.ty {
                        TypeSpec::Symbol => {
                            quote! {
                                let node = self.node(HirId::from_raw(node.raw()));
                                let sym = SymId::from_raw(node.lhs);
                                assert!(!sym.is_zero(), "binding symbol is zero");
                                self.symbol(sym)
                            }
                        }
                        TypeSpec::OptionSymbol => {
                            quote! {
                                let node = self.node(HirId::from_raw(node.raw()));
                                let sym = SymId::from_raw(node.lhs);
                                if sym.is_zero() {
                                    None
                                } else {
                                    Some(self.symbol(sym))
                                }
                            }
                        }
                        _ => unreachable!(),
                    };
                    (ctor_sig, ctor_body, accessor_sig, accessor_body)
                }
                LayoutKind::ListRange => {
                    let field = node.fields.first().expect("ListRange requires a field");
                    let elem_ty = type_to_id(&field.ty);
                    let wrapper = list_wrapper_for(&field.ty).map(|(w, _)| w).unwrap();
                    let ctor_sig = quote! {
                        pub fn #ctor_name(&mut self, items: impl IntoIterator<Item = #elem_ty>) -> #node_id
                    };
                    let ctor_body = quote! {
                        let (start, end) = self.alloc_list(items.into_iter().map(|it| it.raw()));
                        let hir = self.push_node(NodeKind::#kind_variant, start.raw(), end.raw());
                        #node_id::from_raw(hir.raw())
                    };
                    let accessor_sig =
                        quote! { pub fn #accessor_name(&self, node: #node_id) -> #wrapper<'_> };
                    let accessor_body = quote! {
                        let node = self.node(HirId::from_raw(node.raw()));
                        assert_eq!(node.kind, NodeKind::#kind_variant);
                        let start = IxId::from_raw(node.lhs).get();
                        let end = IxId::from_raw(node.rhs).get();
                        #wrapper { slice: &self.node_ids[start..end] }
                    };
                    (ctor_sig, ctor_body, accessor_sig, accessor_body)
                }
                LayoutKind::CallRange => {
                    let callee = &node.fields[0];
                    let args = &node.fields[1];
                    let callee_ty = type_to_id(&callee.ty);
                    let args_ty = type_to_id(&args.ty);
                    let wrapper = list_wrapper_for(&args.ty).map(|(w, _)| w).unwrap();
                    let ctor_sig = quote! {
                        pub fn #ctor_name(&mut self, callee: #callee_ty, args: impl IntoIterator<Item = #args_ty>) -> #node_id
                    };
                    let ctor_body = quote! {
                        let (start, end) = self.alloc_list(std::iter::once(callee.raw()).chain(args.into_iter().map(|it| it.raw())));
                        let hir = self.push_node(NodeKind::#kind_variant, start.raw(), end.raw());
                        #node_id::from_raw(hir.raw())
                    };
                    let accessor_sig = quote! { pub fn #accessor_name(&self, node: #node_id) -> (#callee_ty, #wrapper<'_>) };
                    let accessor_body = quote! {
                        let node = self.node(HirId::from_raw(node.raw()));
                        assert_eq!(node.kind, NodeKind::#kind_variant);
                        let start = IxId::from_raw(node.lhs).get();
                        let end = IxId::from_raw(node.rhs).get();
                        let ids = &self.node_ids[start..end];
                        let (callee, args) = ids.split_first().expect("Call node must have at least one element");
                        (#callee_ty::from_raw(crate::hir::id::Raw(*callee)), #wrapper { slice: args })
                    };
                    (ctor_sig, ctor_body, accessor_sig, accessor_body)
                }
                LayoutKind::BlockWithTail => {
                    let list_field = &node.fields[0];
                    let tail_field = &node.fields[1];
                    let list_ty = type_to_id(&list_field.ty);
                    let tail_ty = type_to_id(&tail_field.ty);
                    let wrapper = list_wrapper_for(&list_field.ty).map(|(w, _)| w).unwrap();
                    let ctor_sig = quote! {
                        pub fn #ctor_name(&mut self, items: impl IntoIterator<Item = #list_ty>, tail: #tail_ty) -> #node_id
                    };
                    let ctor_body = quote! {
                        let iter = items.into_iter().map(|it| it.raw()).chain(std::iter::once(tail.raw()));
                        let (start, end) = self.alloc_list(iter);
                        let hir = self.push_node(NodeKind::#kind_variant, start.raw(), end.raw());
                        #node_id::from_raw(hir.raw())
                    };
                    let accessor_sig = quote! { pub fn #accessor_name(&self, node: #node_id) -> (#wrapper<'_>, #tail_ty) };
                    let accessor_body = quote! {
                        let node = self.node(HirId::from_raw(node.raw()));
                        assert_eq!(node.kind, NodeKind::#kind_variant);
                        let start = IxId::from_raw(node.lhs).get();
                        let end = IxId::from_raw(node.rhs).get();
                        let ids = &self.node_ids[start..end];
                        let (tail, head) = ids.split_last().expect("Block-like node must have a tail element");
                        (#wrapper { slice: head }, #tail_ty::from_raw(crate::hir::id::Raw(*tail)))
                    };
                    (ctor_sig, ctor_body, accessor_sig, accessor_body)
                }
                LayoutKind::TripleLane => {
                    let fields = &node.fields;
                    let a = &fields[0];
                    let b = &fields[1];
                    let c = &fields[2];
                    let a_ty = type_to_id(&a.ty);
                    let b_ty = type_to_id(&b.ty);
                    let c_ty = type_to_id(&c.ty);
                    let args = fields.iter().map(|field| {
                        let name = &field.name;
                        let arg_ty = type_to_arg(field.arg_ty.as_ref().unwrap_or(&field.ty));
                        quote! { #name: #arg_ty }
                    });
                    let ctor_sig = quote! { pub fn #ctor_name(&mut self, #(#args),*) -> #node_id };
                    let a_raw = arg_to_raw(a);
                    let b_raw = arg_to_raw(b);
                    let c_raw = arg_to_raw(c);
                    let ctor_body = quote! {
                        let a_raw = #a_raw;
                        let b_raw = #b_raw;
                        let c_raw = #c_raw;
                        let start = self.alloc_triple(a_raw, b_raw, c_raw);
                        let rhs = IxId::new(start.get() + 1);
                        let hir = self.push_node(NodeKind::#kind_variant, start.raw(), rhs.raw());
                        #node_id::from_raw(hir.raw())
                    };
                    let accessor_sig = if let Some(struct_name) = view_struct_name(&node.name) {
                        quote! { pub fn #accessor_name(&self, node: #node_id) -> #struct_name }
                    } else {
                        quote! { pub fn #accessor_name(&self, node: #node_id) -> (#a_ty, #b_ty, #c_ty) }
                    };
                    let accessor_body = {
                        let a_name = &a.name;
                        let b_name = &b.name;
                        let c_name = &c.name;
                        let a_from =
                            quote! { #a_ty::from_raw(crate::hir::id::Raw(self.node_ids[start])) };
                        let b_from = quote! { #b_ty::from_raw(crate::hir::id::Raw(self.node_ids[start + 1])) };
                        let c_from = quote! { #c_ty::from_raw(crate::hir::id::Raw(self.node_ids[start + 2])) };
                        if let Some(struct_name) = view_struct_name(&node.name) {
                            quote! {
                                let node = self.node(HirId::from_raw(node.raw()));
                                assert_eq!(node.kind, NodeKind::#kind_variant);
                                let start = IxId::from_raw(node.lhs).get();
                                #struct_name { #a_name: #a_from, #b_name: #b_from, #c_name: #c_from }
                            }
                        } else {
                            quote! {
                                let node = self.node(HirId::from_raw(node.raw()));
                                assert_eq!(node.kind, NodeKind::#kind_variant);
                                let start = IxId::from_raw(node.lhs).get();
                                (#a_from, #b_from, #c_from)
                            }
                        }
                    };
                    (ctor_sig, ctor_body, accessor_sig, accessor_body)
                }
                LayoutKind::Direct2 => {
                    let fields = &node.fields;
                    let a = &fields[0];
                    let b = &fields[1];
                    let a_ty = type_to_id(&a.ty);
                    let b_ty = type_to_id(&b.ty);
                    let args = fields.iter().map(|field| {
                        let name = &field.name;
                        let arg_ty = type_to_arg(field.arg_ty.as_ref().unwrap_or(&field.ty));
                        quote! { #name: #arg_ty }
                    });
                    let ctor_sig = quote! { pub fn #ctor_name(&mut self, #(#args),*) -> #node_id };
                    let a_raw = arg_to_raw(a);
                    let b_raw = arg_to_raw(b);
                    let ctor_body = quote! {
                        let a_raw = #a_raw;
                        let b_raw = #b_raw;
                        let hir = self.push_node(NodeKind::#kind_variant, a_raw, b_raw);
                        #node_id::from_raw(hir.raw())
                    };
                    let accessor_sig = if let Some(struct_name) = view_struct_name(&node.name) {
                        quote! { pub fn #accessor_name(&self, node: #node_id) -> #struct_name }
                    } else {
                        quote! { pub fn #accessor_name(&self, node: #node_id) -> (#a_ty, #b_ty) }
                    };
                    let accessor_body = {
                        let a_name = &a.name;
                        let b_name = &b.name;
                        let a_from = quote! { #a_ty::from_raw(node.lhs) };
                        let b_from = quote! { #b_ty::from_raw(node.rhs) };
                        if let Some(struct_name) = view_struct_name(&node.name) {
                            quote! {
                                let node = self.node(HirId::from_raw(node.raw()));
                                assert_eq!(node.kind, NodeKind::#kind_variant);
                                #struct_name { #a_name: #a_from, #b_name: #b_from }
                            }
                        } else {
                            quote! {
                                let node = self.node(HirId::from_raw(node.raw()));
                                assert_eq!(node.kind, NodeKind::#kind_variant);
                                (#a_from, #b_from)
                            }
                        }
                    };
                    (ctor_sig, ctor_body, accessor_sig, accessor_body)
                }
                LayoutKind::ZeroZero => {
                    let ctor_sig = quote! { pub fn #ctor_name(&mut self) -> #node_id };
                    let ctor_body = quote! {
                        let hir = self.push_node(NodeKind::#kind_variant, crate::hir::id::Raw::ZERO, crate::hir::id::Raw::ZERO);
                        #node_id::from_raw(hir.raw())
                    };
                    let accessor_sig = quote! {};
                    let accessor_body = quote! {};
                    (ctor_sig, ctor_body, accessor_sig, accessor_body)
                }
            };

            constructors.push(quote! {
                #ctor_sig {
                    #ctor_body
                }
            });

            if !matches!(layout, LayoutKind::ZeroZero) {
                accessors.push(quote! {
                    #accessor_sig {
                        #accessor_body
                    }
                });
            }
        }

        let node_kind_enum = quote! {
            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            pub enum NodeKind {
                #(#node_kind_variants,)*
            }
        };

        let node_id_trait = quote! {
            pub trait NodeId: sealed::Sealed + Copy {
                fn raw(self) -> u32;
            }

            mod sealed {
                pub trait Sealed {}
            }
        };

        let node_store_impl = quote! {
            impl<'db> crate::hir::store::NodeStore<'db> {
                #(#constructors)*
                #(#accessors)*
                #(#downcasts)*

                pub fn node_kind(&self, node: impl NodeId) -> NodeKind {
                    let hir = HirId::from_raw(crate::hir::id::Raw(node.raw()));
                    self.node_kind_raw(hir)
                }
            }
        };

        let upcasts_tokens = quote! { #(#upcasts)* };
        let node_id_impl_tokens =
            quote! { #(#node_id_trait_impls)* #(#node_id_trait_impls_categories)* };

        quote! {
            #node_kind_enum
            #tag_defs
            #node_id_trait
            #node_id_impl_tokens
            #upcasts_tokens
            #(#view_structs)*
            #(#list_wrappers)*
            #node_store_impl
        }
    }
}

fn format_type(ty: &TypeSpec) -> String {
    match ty {
        TypeSpec::Symbol => "Symbol".to_string(),
        TypeSpec::OptionSymbol => "OptionSymbol".to_string(),
        TypeSpec::Node(ident) => ident.to_string(),
    }
}

fn list_wrapper_for(ty: &TypeSpec) -> Option<(Ident, proc_macro2::TokenStream)> {
    match ty {
        TypeSpec::Node(ident) => {
            let id = format_ident!("{}Id", ident);
            let wrapper = format_ident!("{}Ids", ident);
            Some((wrapper, quote! { #id }))
        }
        _ => None,
    }
}

fn type_to_id(ty: &TypeSpec) -> proc_macro2::TokenStream {
    match ty {
        TypeSpec::Symbol => quote! { mitki_span::Symbol<'db> },
        TypeSpec::OptionSymbol => quote! { Option<mitki_span::Symbol<'db>> },
        TypeSpec::Node(ident) => {
            let id = format_ident!("{}Id", ident);
            quote! { #id }
        }
    }
}

fn type_to_arg(ty: &TypeSpec) -> proc_macro2::TokenStream {
    match ty {
        TypeSpec::Symbol => quote! { mitki_span::Symbol<'db> },
        TypeSpec::OptionSymbol => quote! { Option<mitki_span::Symbol<'db>> },
        TypeSpec::Node(ident) => {
            let id = format_ident!("{}Id", ident);
            quote! { #id }
        }
    }
}

fn view_struct_name(node: &Ident) -> Option<Ident> {
    match node.to_string().as_str() {
        "Binary" => Some(format_ident!("BinaryExpr")),
        "If" => Some(format_ident!("IfExpr")),
        "Prefix" => Some(format_ident!("PrefixExpr")),
        "Postfix" => Some(format_ident!("PostfixExpr")),
        "LocalVar" => Some(format_ident!("LocalVar")),
        _ => None,
    }
}

fn to_snake(name: &str) -> String {
    let mut out = String::new();
    for (i, ch) in name.chars().enumerate() {
        if ch.is_ascii_uppercase() {
            if i != 0 {
                out.push('_');
            }
            out.push(ch.to_ascii_lowercase());
        } else {
            out.push(ch);
        }
    }
    out
}

fn constructor_suffix(name: &Ident) -> String {
    match name.to_string().as_str() {
        "TypePath" => "type_ref".to_string(),
        other => to_snake(other),
    }
}

fn accessor_name(name: &Ident) -> String {
    match name.to_string().as_str() {
        "If" => "if_expr".to_string(),
        "Closure" => "closure_parts".to_string(),
        "Block" => "block_stmts".to_string(),
        "TypePath" => "type_ref".to_string(),
        other => to_snake(other),
    }
}

fn arg_to_raw(field: &FieldDef) -> proc_macro2::TokenStream {
    let name = &field.name;
    let arg_ty = field.arg_ty.as_ref().unwrap_or(&field.ty);
    match (arg_ty, &field.ty) {
        (TypeSpec::Symbol, TypeSpec::Node(store)) if store == "Name" => {
            quote! { self.alloc_name(#name).raw() }
        }
        (TypeSpec::Symbol, _) => quote! { self.intern_symbol(#name).raw() },
        (TypeSpec::OptionSymbol, _) => quote! {
            #name.map_or(crate::hir::id::Raw::ZERO, |sym| self.intern_symbol(sym).raw())
        },
        (TypeSpec::Node(_), _) => quote! { #name.raw() },
    }
}

// No helpers needed; generated IntoIterator uses per-wrapper map_fn.
