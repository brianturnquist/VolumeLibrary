# National Volume Estimator Library: mFVS fork

This repository is a fork of the USDA Forest Service [National Volume Estimator Library](https://github.com/FMSC-Measurements/VolumeLibrary) (NVEL), which provides the tree volume, taper and biomass equations used by the Forest Vegetation Simulator (FVS). It is maintained as the `volume/NVEL` submodule of [mFVS](https://github.com/brianturnquist/mFVS), a modernization of FVS in modern Fortran.

> **This fork is not the official library.** It is not affiliated with, maintained by, or endorsed by the USDA Forest Service. For the official NVEL, its documentation and support, see [FMSC-Measurements/VolumeLibrary](https://github.com/FMSC-Measurements/VolumeLibrary).

## How it differs from the official library

The fork is the official library's `master` branch as of commit `7ce1fd5` (28 September 2026), with one change. All 130 Fortran source files have been converted from fixed-form `.f` and `.for` to free-form `.f90`, and the include files (`.inc`) have been rewritten in free form under their existing names. The Intel Visual Fortran build files (`comp*.bat` and `vollib/vollib.vfproj`) were updated to the new file names. mFVS does not use them: it compiles the NVEL routines it needs with gfortran as part of each FVS variant. [readme.txt](readme.txt) is kept unchanged from the official library.

## Same results

The conversion is meant to change the form of the code, not its results. It is checked through the mFVS regression tests. Built with this fork, 22 of the 24 FVS variants produce output identical to mFVS's pre-modernization reference. That reference used an earlier NVEL commit (`3854807`), so the comparison also covers the official library's fixes from August and September 2026. The BC and ON variants cannot be checked yet (see the mFVS [FOUND_BUGS.md](https://github.com/brianturnquist/mFVS/blob/main/FOUND_BUGS.md)). NVEL routines that FVS does not call, such as the stand-alone DLL entry points, are not covered by these tests.

## Bringing in changes from the official library

Every source file has been renamed, so git cannot merge later changes from the official library automatically. Each one must be ported by hand to the matching `.f90` file, then checked with the mFVS regression tests. To see what the official library has changed since this fork was made:

```sh
git remote add upstream https://github.com/FMSC-Measurements/VolumeLibrary.git
git fetch upstream
git log --oneline 7ce1fd5..upstream/master
```

## Contributing, security and license

Changes to this fork follow the mFVS [contributing guidelines](https://github.com/brianturnquist/mFVS/blob/main/Contributing.md). Problems with the volume equations themselves are best also reported to the official library, so that all of its users benefit. To report a security problem, follow the mFVS [security policy](https://github.com/brianturnquist/mFVS/blob/main/Security.md).

The official library is in the public domain, and this fork's changes are dedicated to the public domain under CC0 1.0; see [LICENSE.md](LICENSE.md) and [CONTRIBUTING.md](CONTRIBUTING.md). A few individual routines carry their own copyright notices.
