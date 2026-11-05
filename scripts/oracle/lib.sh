# Shared by scripts/oracle/{build,fetch,run}.sh: the lock reader and the artifact format.
#
# THE ARTIFACT is a gzip'd tar, made byte-reproducibly (sorted names, mtime 0, uid/gid 0, gzip -n):
#   bin/concrete      the oracle binary (its sha256 is the lock's binary_sha256)
#   std/              the standard library at the locked commit (the binary reads it at run time)
#   shell/flake.nix shell/flake.lock shell/lean-toolchain
#                     the tag's dev shell, so the C toolchain that compiles programs is the locked one
#                     (a directory of its own: `nix develop path:shell` copies only these to the store)
#   MANIFEST.sha256   `sha256sum` lines for every other file in the artifact
# Its own sha256 is the lock's artifact_sha256, and MANIFEST.sha256's own sha256 is the lock's
# manifest_sha256. An installed artifact is a directory with exactly these regular files (and their
# directories, nothing else: no symlinks); oracle_verify_install anchors the manifest in the lock, so
# every installed file (std/, shell/ as well as bin/concrete) is authenticated by the reviewed lock.
ORACLE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ORACLE_LOCK="$ORACLE_DIR/ORACLE.lock"
lock() { sed -n "s/^$1=//p" "$ORACLE_LOCK" | head -1; }
oracle_home() { echo "${ORACLE_HOME:-$HOME/oracle}"; }
oracle_artifact_name() { echo "oracle-$(lock tag)-$(lock platform).tar.gz"; }

# oracle_pack STAGE OUT: write the artifact for staged directory STAGE (which must hold the files above
# except MANIFEST.sha256, which this adds) to OUT.
oracle_pack() {
  local stage="$1" out="$2"
  ( cd "$stage" && rm -f MANIFEST.sha256 && find . -type f ! -name MANIFEST.sha256 | LC_ALL=C sort \
      | sed 's#^\./##' | xargs -d '\n' sha256sum > MANIFEST.sha256 ) || return 1
  tar --sort=name --mtime=@0 --owner=0 --group=0 --numeric-owner --format=gnu \
      --mode='u+rwX,go+rX,go-w' -C "$stage" -cf - . | gzip -n -9 > "$out"
}

# oracle_verify_install DIR: exit 0 iff DIR is an intact install (prints the reason otherwise).
oracle_verify_install() {
  local d="$1" want got
  [ -f "$d/MANIFEST.sha256" ] && [ -x "$d/bin/concrete" ] || { echo "no installed artifact at $d"; return 1; }
  [ ! -L "$d" ] || { echo "$d is a symlink"; return 1; }
  want="$(lock manifest_sha256)"; got="$(sha256sum "$d/MANIFEST.sha256" | cut -d' ' -f1)"
  [ -n "$want" ] && [ "$got" = "$want" ] || { echo "MANIFEST.sha256 is $got, the lock says ${want:-nothing}"; return 1; }
  # Only regular files and directories: a symlink (or fifo, device...) would escape the inventory.
  local odd; odd="$(cd "$d" && find . ! -type f ! -type d | head -1)"
  [ -z "$odd" ] || { echo "$d holds a non-regular entry: $odd"; return 1; }
  ( cd "$d" && sha256sum --quiet --strict -c MANIFEST.sha256 ) >/dev/null 2>&1 || { echo "a file in $d differs from its manifest"; return 1; }
  # Nothing added: the files present are exactly the manifest's plus the manifest itself.
  local listed present
  listed="$( (sed 's/^[0-9a-f]*  //' "$d/MANIFEST.sha256"; echo MANIFEST.sha256) | LC_ALL=C sort)"
  present="$(cd "$d" && find . -type f | sed 's#^\./##' | LC_ALL=C sort)"
  [ "$listed" = "$present" ] || { echo "files in $d are not exactly the manifest's (added or missing)"; return 1; }
  want="$(lock binary_sha256)"; got="$(sha256sum "$d/bin/concrete" | cut -d' ' -f1)"
  [ -n "$want" ] && [ "$got" = "$want" ] || { echo "bin/concrete is $got, the lock says ${want:-nothing}"; return 1; }
}

# oracle_install TARBALL DEST: verify the tarball's sha256 against the lock, unpack, verify the install.
oracle_install() {
  local tb="$1" dest="$2" want got
  want="$(lock artifact_sha256)"; got="$(sha256sum "$tb" | cut -d' ' -f1)"
  [ -n "$want" ] && [ "$got" = "$want" ] || { echo "artifact is $got, the lock says ${want:-nothing}"; return 1; }
  rm -rf "$dest.new" && mkdir -p "$dest.new" && tar -xzf "$tb" -C "$dest.new" || { echo "cannot unpack $tb"; return 1; }
  oracle_verify_install "$dest.new" || return 1
  rm -rf "$dest.old"; [ ! -e "$dest" ] || mv "$dest" "$dest.old"
  mv "$dest.new" "$dest" && rm -rf "$dest.old"
}
