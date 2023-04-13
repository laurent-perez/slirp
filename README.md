# SLIRP
This a patched version of [SLIRP](https://space.mit.edu/cxc/slirp/),
originally written by Michael S. Noble.

SLIRP is now maintained by John E. Davis and this version is based on pre-2.0.0.34
snapshop available [here](https://www.jedsoft.org/snapshots/).

Those modifications to SLIRP code were made to make projects like
[slgtk3](https://github.com/laurent-perez/slgtk3) or
[slcairo](https://github.com/laurent-perez/slcairo) to be built ; some
C constructs in newer versions of GTK3, Glib, Pango... not being
correctly understood by original SLIRP.

They should eventually be integrated to SLIRP main code when we will
be sure that they do not introduce unwanted side effects.

You may either install SLIRP (see INSTALL.txt) or just copy `slirp.sl
, slirpmaps.sl and slirputils.sl` to an existing installation.

## Changes made so far :

### Constants.

Make those kind of constants parsed properly :

`#define PANGO_SCALE_X_LARGE  ((double)1.44)`

### Enumerations.

Some enum declarations may contain embeded comment, like this
example from `gioenums.h` that will not beeing parsed properly :

```
typedef enum /*< flags >*/ {`
   G_MOUNT_MOUNT_NONE = 0`
} GMountMountFlags;
```

Match those kind of enumeration from SDL2 :

```
SDLK_0 = '0'
SDLK_BACKSLASH = '\\'
SDLK_QUOTE = '\''
```

### Discard lines.

Some gtk header files end with 1 (or more) line(s) without a ";" which
will confuse the parser. This patch add a new function
`slirp_discard_line ("str")` to ask slirp to remove a line given
the string `str`.

`G_DEFINE_AUTOPTR_CLEANUP_FUNC(GtkAboutDialog, g_object_unref)`

### Returns intrinsic structures.

Add a new statement to instruct slirp that a function can return an
intrinsic S-Lang structure.

`returns_struct ["SomeFunc"] = [1];`

### Miscellaneous.

Avoid this warning : "Confusing syntax seen for enum..."

Correct a small typo in help message.

The SLIRP_VERSION_STRING as been bumped to pre2.0.0-35
