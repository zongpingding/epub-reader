<h1 align="center">
  <a href="https://github.com/zongpingding/epub-reader.git">EPub Reader</a>
</h1>

A custom EPUB reader built with Haskell and GTK.

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
`epub-reader` will automatically generate a `config.toml` file in `~/.config/epub-reader` on first launch, check it for details.


## TODO
Here are some pending tasks:

- [ ] The critical issue of text being clipped still needs improvement, since it is related to overlap, spacing, and font size. One possible solution might be to borrow ideas from TeX's pagination algorithm: if the first or last line truly cannot fit, simply move it to the next page.
- [ ] Support navigation for the table of contents and internal hyperlinks.
- [ ] Provide several built-in reading themes.
- [ ] Optimize memory usage. It currently seems to consume around 300 MB(for large EPub files), while the target is to keep it below 100 MB.
- [x] Support collapsing the table of contents.
- [x] Use TOML as the configuration format.
- [x] Improve the table of contents feature: extraction of secondary headings was too rough, and secondary headings may also need indentation.
- [x] Allow manually selecting a file to open each time, similar to other readers.
- [x] Add header and footer support, with customization options.
