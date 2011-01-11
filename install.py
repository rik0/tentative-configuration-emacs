import errno
import os
import shutil
import sys

from os import path

def is_windows():
    return os.name == 'nt'

def windows_home():
    return '%(HOMEDRIVE)s%(HOMEPATH)' % os.environ

def home_path():
    return windows_home() if is_windows() else os.environ['HOME']

def find_base():
    if path.isfile('emacs'):
        return os.curdir
    else:
        # here add better diagnostic
        raise RuntimeError("Can't find configuration files.")

def overwrite_if_more_recent(command, src, dest, *args, **kw):
    try:
        command(src, dest, *args, **kw)
    except OSError, e:
        if e.errno == errno.EEXIST:
            try:
                shutil.rmtree(dest)
            except OSError:
                if path.islink(dest):
                    # bloody symlinks
                    os.remove(dest)
            command(src, dest, *args, **kw)

HOME = home_path()
BASE_DIR = find_base()
overwrite_if_more_recent(shutil.copyfile, 'emacs', path.join(HOME, '.emacs'))
overwrite_if_more_recent(
    shutil.copytree, 'emacs.d',
    path.join(HOME, '.emacs.d'),
    ignore=shutil.ignore_patterns('*~', '*.elc')
)

