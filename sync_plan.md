<!-- -*- mode: text; fill-column: 70; -*- -->
Master plan for syncing Emacs setup across environments.

# Install emacs 24


## Ubuntu/Mint

1. `sudo apt-get emacs24, emacs24-bin-common, emacs24-common, emacs24-common-non-dfsg, emacs24-el, emacsen-common`

## RedHat/CentOS
    
1. `curl/wget http://pj.freefaculty.org/EL/PaulJohnson-BinaryPackageSigningKey`
        
2. `sudo mv PaulJohnson-BinaryPackageSigningKey /etc/pki/rpm-gpg`
        
3. `curl/wget http://pj.freefaculty.org/EL/pjku.repo`
        
4. `sudo cp pjku.repo /etc/yum.repos.d`
        
5. `sudo yum install emacs-nox-24.3-12.pjku.*arch* emacs-debuginfo-24.3-12.pjku.*arch*`

       `emacs-common-24.3-12.pjku.*arch* emacs-24.3-12.pjku.*arch*.rpm`
                
## Windows
    
1. Get windows binary from http://ftpmirror.gnu.org/emacs/windows/emacs-24.3-bin-i386.zip.
        
2. Untar into subdir of "Program Files" or "Program Files(X86)".
        
3. If not already done, set up environment variable %HOME% to point to user directory.
        
# Set Up to Synchronize Files
        
1. Install/set up a file sharing system (such as Box, Copy, Google
Drive, whatever M$ calls theirs, or Apple's flavor of the month).
(Dropbox not recommended; google #DropDropbox).

2. make ~/.emacs a symbolic link to [synch folder]/emacs/.emacs
(for Windows: Create %HOME%/.emacs (or _emacs) file which
loads [synch folder]/emacs/.emacs)
    
3. create ~/.emacs-site.el for local customizations. Set the value
of symbol emacs-sync-directory to the synch folder (parent of
emacs/).

4. Start emacs. .emacs will load [synch folder]/emacs/init.el,
which will load [synch folder]/emacs/sync_packages.el, which
will automatically install a set of packages.

    a. debug problems which will inevitably occur

# Profit!
