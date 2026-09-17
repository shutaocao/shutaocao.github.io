# Building shutaocao.github.io with Academic Pages

Internal reference notes — not part of the published site (this `notes/` folder
is excluded from the Jekyll build via `_config.yml`).

Steps I followed to build my personal page on GitHub Pages using the
[Academic Pages](https://github.com/academicpages/academicpages.github.io)
template. Everything below is done from a terminal on Fedora Linux.

## Install Ruby, Node, Jekyll

```bash
sudo dnf install ruby node bundler ruby-devel jekyll
```

## Clone the template

All source files live in `~/myPage`.

```bash
git clone https://github.com/academicpages/academicpages.github.io.git myPage
cd myPage
git submodule update --init --recursive   # if the template adds submodules later
```

Point the local repo's `origin` at my own GitHub Pages repo instead of the
template's:

```bash
git remote set-url origin git@github.com:shutaocao/shutaocao.github.io.git
git branch -M main
git push -u origin main
```

On github.com: make the `shutaocao.github.io` repo public, then in
**Settings → Pages**, set the source branch to `main`.

## Install dependencies and preview locally

```bash
bundle install
bundle exec jekyll serve
```

Serves the compiled site from `_site/` at http://127.0.0.1:4000.

### Ruby 3.4+ / 4.x compatibility

The `github-pages` gem (used so the local build matches GitHub's own Jekyll
version) pins very old dependencies — `jekyll 3.9.0` and `liquid ~4.0` — that
predate several breaking changes in modern Ruby. On a fresh install (e.g.
Fedora 44, which ships Ruby 4.0), `bundle exec jekyll serve` fails with a
chain of errors unless two things are in place:

1. **`Gemfile`** explicitly declares `csv` and `bigdecimal` — these were
   removed from Ruby's default gems in 3.4+, and old Liquid requires them
   directly.
2. **`_plugins/ruby32_taint_shim.rb`** restores harmless no-op stubs for
   `Object#tainted?`/`#untaint` (removed in Ruby 3.2, but still called by old
   Liquid) and fixes a keyword-argument bug in the `pathutil` gem (Ruby 3.0
   stopped auto-converting a trailing hash into keyword args, which broke
   Jekyll's WSL-detection check during `serve`). Jekyll auto-loads
   `_plugins/*.rb`, so this only needs to exist once — no extra require
   needed.

Neither of these affects the deployed site: GitHub Pages builds with its own
environment regardless of the local Gemfile.

## Folder structure

- `_data/navigation.yml` — top menu. Add/remove/comment-out items here.
- `_pages/` — standalone pages (About, Research, Teaching, CV, ...). One
  Markdown/HTML file per page, referenced by `permalink:` in its front matter.
- `_publications`, `_talks`, `_posts`, `_teaching`, `_portfolio` — content
  collections. One item per file. A collection only shows up if something
  links to its archive page (see `_data/navigation.yml`) or its items are
  referenced elsewhere.
- `files/` — static downloads (PDFs, course materials, data files),
  referenced from pages with relative links like `/files/...`.
- `notes/` — this file. Excluded from the Jekyll build.

Current menu (`_data/navigation.yml`):

1. **Research** → `_pages/research.md` — hand-written page (not the
   `_publications` collection; I list papers directly since I want manual
   control over grouping/formatting). Uses `layout: archive`, which pulls in
   the theme's sidebar/author bio automatically — no different from any other
   collection-driven archive page.
2. **Teaching** → `_teaching/` + `_pages/teaching.md`.
3. **Data Project** → `_portfolio/`.
4. **Posts** → `_posts/` (via `_pages/year-archive.html`).

`_pages/codes.md`, `_pages/cv.md`, and the `_talks` collection exist as
inactive placeholders (not linked in the nav) for whenever I'm ready to fill
them in.

For publications, I originally planned to use
`markdown_generator/publications.ipynb` to auto-generate `_publications/*.md`
from a bibliography — I ended up hand-writing `research.md` instead, so that
generator and the `_publications` collection are unused for now.

## Deploying updates

```bash
bundle exec jekyll serve   # sanity-check locally first
git add .
git commit -m "update ..."
git push origin main
```

If `git push` is rejected because the remote has commits I don't have
locally (e.g. edited a file directly on github.com):

```bash
git pull --rebase origin main
git push origin main
```

Avoid `git push -f`: it overwrites whatever is on the remote instead of
resolving the divergence, and any real conflict will just resurface next
time.

### Acknowledgement

Template: [Academic Pages](https://github.com/academicpages/academicpages.github.io).
