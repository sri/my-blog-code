# author: defcraft.org/~sri
# released under same license as python 2.5
import sys, os, datetime, traceback, commands

specialcmds = {}
allcmds = []

class CmdMeta(type):
    def __init__(cls, name, bases, dict):
        super(CmdMeta, cls).__init__(cls, name, bases, dict)
        if name == 'Cmd': return
        simplecmds = {}
        specials = {}
        inst = cls()
        inst.install(specials, simplecmds)
        for k in simplecmds:
            setattr(__builtins__, k, SimpleCmd(simplecmds[k]))
        allcmds.append((name, cls, inst, simplecmds, specials))
        specialcmds.update(specials)

class Cmd(object):
    __metaclass__ = CmdMeta

class SimpleCmd(object):
    def __init__(self, func):
        self.func = func
    def __call__(self):
        result = self.func()
        if result:
            return str(result)

def myexcepthook(type, value, tb):
    # imports giving rise to syntaxerror shouldn't
    # be considered as cmds
    if type is SyntaxError and value.filename=='<stdin>':
        # might be a cmd of ours
        line = value.text
        # if user is entering a multiline expr
        # (line[0]!=' ') -- they indent everything
        # other than first line -- don't consider
        # that a cmd
        if line and line[0] not in ' \t':
            cmd = value.text.split()
            if cmd and cmd[0] in specialcmds:
                fn = specialcmds[cmd[0]]
                arg = line[len(cmd[0]):]
                try:
                    fn(arg)
                except Exception, e:
                    print 'error when running cmd %s' % cmd[0]
                    print repr(e)
                return
    traceback.print_exception(type, value, tb)
sys.excepthook = myexcepthook


# reloading this file shouldn't mess up prev value
try:
    __builtins__._
except AttributeError:
    __builtins__._ = None


def mydisplayhook(value):
    if value is not None:
        # this'll only invoke simplecmds (or
        # cmds that install themselves in
        # __builtins__) -- the specialcmds are
        # executed in myexcepthook
        if isinstance(value, SimpleCmd):
            out = value()
            if out:
                print out
        else:
            __builtins__._ = value
            print repr(value)
sys.displayhook = mydisplayhook


#
# Commands:
#
class MyCmds(Cmd):
    def install(self, specialcmds, simplecmds):
        simplecmds['cmds'] = self.allcmds
    def doc(self, x):
        return getattr(x, '__doc__') or '(no doc)'
    def allcmds(self):
        "prints out all the cmds"
        print "all commands\n", 65*'='
        cmds = []
        for (name, cls, inst, simplecmds, specialcmds) in allcmds:
            simple = set(simplecmds.keys())
            specials = set(specialcmds.keys())
            both = simple & specials
            simple -=  both
            specials -= both
            for k in both:
                cmds.append((k, 'both', self.doc(simplecmds[k])))
            for k in specials:
                cmds.append((k, 'special', self.doc(specialcmds[k])))
            for k in sorted(simple):
                cmds.append((k, 'simple', self.doc(simplecmds[k])))
        cmds.sort()
        for x in cmds:
            print '%-6s  %-7s    %s' % x
        

class MyReload(Cmd):
    def install(self, specialcmds, simplecmds):
        simplecmds['rl'] = self.doit
    def doit(self):
        "reloads your pythonrc startup file"
        path = os.environ.get('PYTHONSTARTUP')
        if not path:
            print "can't find your python startup file"
        else:
            execfile(path, globals())

class MyPwd(Cmd):
    def install(self, specialcmds, simplecmds):
        simplecmds['pwd'] = simplecmds['cwd'] = self.getcwd
    def getcwd(self):
        "prints the current working directory"
        return os.getcwd()

class MyCD(Cmd):
    def __init__(self):
        self.popdirs = []
    def install(self, specialcmds, simplecmds):
        simplecmds['popd'] = self.popd
        simplecmds['cd'] = specialcmds['cd'] = self.cd
    def popd(self):
        "changes back to directory from which we came"
        if self.popdirs:
            self.cd(self.popdirs.pop(), False)
        else:
            print 'nothing to pop back to'
    def cd(self, dir=None, insert=True):
        "changes to a given directory or user home; resolves ~ to user home"
        if not dir:
            import user
            dir = user.home
        dir = os.path.expanduser(dir.strip())
        if not os.path.isdir(dir):
            print 'not a dir:', dir
        else:
            if insert:
                self.popdirs.append(os.getcwd())
            os.chdir(dir)
            print 'changed to', dir

class MyHelp(Cmd):
    def install(self, specialcmds, simplecmds):
        specialcmds['h'] = self.help
    def help(self, x):
        "prints help to a given thing"
        help(eval(x.strip()))


# TODO: make better
class MyPdb(Cmd):
    def install(self, specialcmds, simplecmds):
        specialcmds['pdb'] = self.pdb
    def pdb(self, x):
        import pdb
        x = x.strip()
        if not x.endswith(')'):
            x = x + '()'
        pdb.run(x)

class MyLs(Cmd):
    def install(self, specialcmds, simplecmds):
        specialcmds['ls'] = self.ls
        simplecmds['ls'] = self.ls
    def ls(self, dir=''):
        """run the unix command 'ls -l'"""
        dir = os.path.expanduser(dir.strip() or '.')
        print commands.getoutput('ls -l %s' % dir)


class MyTimeit(Cmd):
    def install(self, specialcmds, simplecmds):
        specialcmds['ti'] = self.timeit
    def timeit(self, s):
        "profile via the timeit module; syntax: 'ti module_to_import fn_name'"
        if len(s.split()) != 2:
            print 'usage: ti module_to_import fn_name'
            return
        mod, fnname = s.split()
        cmd = "%s -mtimeit -s 'import %s' '%s.%s()'" % (
            sys.executable,
            mod,
            mod,
            fnname)
        out = commands.getoutput(cmd)
        print 'profiling results:'
        spaces = ' ' *4
        lines = [spaces+line for line in out.split('\n')]
        print '\n'.join(lines)
