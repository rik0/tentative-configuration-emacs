import errno
import os
import shutil
import sys

from os import path

EMACS_CONF_FILE = 'emacs'
EMACS_CONF_DIR = 'emacs.d'

def hidden_name(name):
    return '.%s' % name

def unhidden_name(name):
    if name.startswith('.'):
        return name[1:]
    else:
        return name

def is_windows():
    return os.name == 'nt'

def windows_home():
    return '%(HOMEDRIVE)s%(HOMEPATH)s' % os.environ

def home_path():
    return windows_home() if is_windows() else os.environ['HOME']

def find_base():
    if path.isfile(EMACS_CONF_FILE):
        return os.curdir
    else:
        # here add better diagnostic
        enclosing_dir = path.dirname(__file__)
        tentative_emacs = path.join(enclosing_dir, EMACS_CONF_FILE)
        if path.isfile(tentative_emacs):
            return enclosing_dir
        else:
            raise RuntimeError("Can't find configuration files.")

# everybody nows that explicit logging this way sucks
def overwrite_if_more_recent(command, src, dest, *args, **kw):
    try:
        command(src, dest, *args, **kw)
        print '%(src)s -> %(dest)s' % locals()
    except OSError, e:
        if e.errno == errno.EEXIST:
            try:
                shutil.rmtree(dest)
                print '[DEL] %s' % dest
            except OSError:
                if path.islink(dest):
                    # bloody symlinks
                    os.remove(dest)
                    print '[DEL SYM] %s' % dest
            command(src, dest, *args, **kw)
            print '%(src)s -> %(dest)s' % locals()

if __name__ == '__main__':
    HOME_DIR = home_path()
    BASE_DIR = find_base()

    EMACS_VERSIONED_FILE = path.join(BASE_DIR, unhidden_name(EMACS_CONF_FILE))
    EMACS_VERSIONED_DIR = path.join(BASE_DIR, unhidden_name(EMACS_CONF_DIR))

    EMACS_ACTIVE_FILE = path.join(HOME_DIR, hidden_name(EMACS_CONF_FILE))
    EMACS_ACTIVE_DIR = path.join(HOME_DIR, hidden_name(EMACS_CONF_DIR))

    try:
        command = sys.argv[1]
    except KeyError:
        command = 'activate'

    if command == 'activate':
        overwrite_if_more_recent(
            shutil.copyfile,
            EMACS_VERSIONED_FILE,
            EMACS_ACTIVE_FILE
        )
        overwrite_if_more_recent(
            shutil.copytree,
            EMACS_VERSIONED_DIR,
            EMACS_ACTIVE_DIR,
            ignore=shutil.ignore_patterns('*~', '*.elc')
        )
    elif 'store':
        overwrite_if_more_recent(
            shutil.copyfile,
            EMACS_ACTIVE_FILE,
            EMACS_VERSIONED_FILE
        )
        overwrite_if_more_recent(
            shutil.copytree,
            EMACS_ACTIVE_DIR,
            EMACS_VERSIONED_DIR,
            ignore=shutil.ignore_patterns('*~', '*.elc')
        )



