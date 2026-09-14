# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0] — 2026-09-15

Initial release.

### Added

- **Platform layer** (`tessera/platform`) — GLFW windowing loaded at
  runtime through the FFI; keyboard, mouse, clipboard, cursors, vsync,
  DPI-aware framebuffer scaling.
- **Renderer** (`tessera/render`) — batched triangle stream over
  client-side vertex arrays; CPU-tessellated rounded rectangles, rings,
  circles, round-cap lines; per-vertex color gradients; scissor stack;
  4x MSAA; alpha-textured glyphs via fixed-function `GL_MODULATE`.
- **Text** (`tessera/text`, `tessera/font`) — TrueType/TTC parsing
  (cmap 0/4/6/12, simple + composite glyphs, hhea/hmtx metrics), scanline
  area rasterizer with 4x vertical subsampling, 2048px lazy glyph atlas,
  per-character CJK font fallback, greedy CJK-aware wrapping.
- **Images** (`tessera/image`) — PNG writer (stored deflate), QOI
  encoder/decoder, BMP and TGA decoders; CRC32/Adler32 exported.
- **View layer** (`tessera/view`, `tessera/layout`) — declarative
  immutable widget tree; measure/arrange layout with flex, align,
  padding; stable node paths; hit-testing.
- **Theme** (`tessera/theme`) — warm light and dark themes.
- **App runtime** (`tessera/app`) — Elm-style `run` loop; GLFW event
  queue; click routing; single-line input with caret, clipboard, and
  focus; posted messages from any thread (`post!`, `quit!`).
- **Snapshots** (`tessera/snapshot`) — headless view-to-PNG rendering for
  tests and documentation.
- Pure-Racket font parsing and image codecs — no C toolchain required.
- Test suite: 38 checks covering codecs, platform, renderer, text, and
  the run loop; snapshot PNGs verified visually.

### Honest gaps (planned for 0.2+)

- Kerning is parsed but not applied (header-variant validation pending).
- CFF/PostScript outline fonts are rejected at load.
- Multi-window, IME composition, and multi-line input are not implemented.
- Linux CI runs headless; the interactive loop is verified on macOS.
