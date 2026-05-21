<h1 align="center">
  <a href="https://github.com/zongpingding/epub-reader.git">EPub Reader</a>
</h1>

A custom EPUB reader built on Haskell and GTK.

## Clone
Clone the repository to your local machine:

```bash
git clone https://github.com/zongpingding/epub-reader.git
cd epub-reader
```

## Build
This project is built using [Stack](https://docs.haskellstack.org/en/stable/).

Before building, ensure you have the necessary system-level C libraries installed for the GUI bindings (e.g., `GTK`). On Arch Linux, you can install the dependencies via:

```bash
sudo pacman -S gtk3 gobject-introspection
```

Set up the Haskell toolchain and build the project:

```bash
stack setup
stack build
```


## Run
Once the build is complete, you can run the application directly using Stack:

```bash
stack run
```

## Install
If there is no issue when running this program, install it with Stack using:

```bash
stack install
```

This will copy the executable binary to `~/.local/bin`.


## Config
`epub-reader` will automatically generate a `config.toml` file in `~/.config/epub-reader` on first launch, check it for details. The default `config.toml` is:

``` toml
# EPUB Reader Configuration File
[window]
show-titlebar = false

[layout]
margin-top = 30
margin-bottom = 30
margin-left = 80
margin-right = 80
line-height = 1.4
block-spacing = 24
bg-color = "#f4ecd8"
font-color = "#2c3e50"
font-family = "Serif"
default-font-size = 18
indent-first = 2

[toc]
line-height = 1.2
selected-bg-color = "#dcd3be"

# Placeholder support: {book_title}, {chapter_name}, {chapter_index}, {total_chapter}, {current_page}, {total_page}
[header]
color = "#8e8e8e"
font-size = 14
left = "{book_title}"
center = ""
right = "{chapter_name}"

[footer]
color = "#8e8e8e"
font-size = 14
left = ""
center = "{current_page} / {total_page}"
right = "CHAPTER {chapter_index} / {total_chapter} TOTAL"
```

Option `selected-bg-color` will apply to (previous) selected toc item.

> If your window-manager(Desktop Environment) has disabled window title bar, then `show-titlebar` has no effect.

## key bindings
Here are some default key bindings for `epub-reader`:

* `Ctrl-o` : open a new EPub file.
* `Ctrl-h` : switch between toc and reading page.
* `Tab`    : toggle toc page.
* `Leftarrow`  : collapse current toc(level).
* `Rightarrow` : expand current toc(level).
* `<Mouse-scroll>` : scroll inside a chapter.
* `Space` : jump to next page.
* `Shift-Space` : jump to previous page.


## TODO
Here are some pending tasks:

- [ ] The critical issue of text being clipped still needs improvement, since it is related to overlap, spacing, and font size. One possible solution might be to borrow ideas from TeX's pagination algorithm: if the first or last line truly cannot fit, simply move it to the next page.
- [ ] Support internal hyperlinks.
- [ ] Provide several built-in reading themes.
- [ ] Optimize memory usage. It currently seems to consume around 300 MB(for large EPub files), while the target is to keep it below 100 MB.
- [x] Support navigation for the table of contents
- [x] Support collapsing the table of contents.
- [x] Use TOML as the configuration format.
- [x] Improve the table of contents feature: extraction of secondary headings was too rough, and secondary headings may also need indentation.
- [x] Allow manually selecting a file to open each time, similar to other readers.
- [x] Add header and footer support, with customization options.
