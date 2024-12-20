# Programmatic Refactoring of Zed

This tool is part of an ongoing effort to simplify context types in the GPUI framework and the Zed app that uses it. It's designed to automatically refactor Rust code, replacing `WindowContext` and `ViewContext` with explicit `Window` parameters along with `AppContext` or `ModelContext`. So far, the tool performs the following transformations:

## Function Signatures
- Replaces `cx: &mut WindowContext` with `window: &mut Window, cx: &mut AppContext`
- Updates function types using `WindowContext` to use `&mut Window` and `&mut AppContext`

## Method Calls
- Replaces `cx.method()` calls with either:
  - `window.method()` for Window-specific methods
  - `cx.method()` for AppContext-specific methods
- Adds `cx` as an additional argument for methods that require it

## Function Calls
- Updates calls to functions that previously took a `WindowContext` parameter
- Replaces the single `WindowContext` argument with both `window` and `cx`

## Imports
- Updates import statements to include both `Window` and `AppContext` where `WindowContext` was previously imported

## Key Features
- Uses tree-sitter for accurate parsing and transformation of Rust code
- Supports both regular functions and methods within impl blocks
- Handles different types of function calls (direct, method, and scoped)
- Provides a dry-run option to preview changes without modifying files
- Processes entire directories of Rust files recursively

## Usage
```
cargo run -- <path_to_rust_files> [--dry-run]
```

This tool aims to simplify the process of updating codebases to use the new `Window` and `AppContext` pattern, reducing manual refactoring effort and potential errors. As a work in progress, it's specifically tailored for the GPUI framework and Zed, focusing on a big migration to simplify the UI framework.
