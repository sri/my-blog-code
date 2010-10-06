import feedparser, cgi, textwrap

html = """
<html>
<head>
<style>
body {
 margin: 30px;
}
table {
 border: solid 1px;
}
.bozo {
 background: pink;
 padding: 2px;
 margin-bottom: 15px;
}
.type {
 color: gray;
 font-size: 11px;
}
</style>
</head>
<body>
%s
</body>
</html>
"""

table = """
<table cellspacing=2 cellpadding=2>
%s
</table>
"""

def bozo(m):
    return '<div class="bozo">%s</div>' % m

def tr(k, v, typ, descend_path):
    if descend_path:
        tooltip = descend_path[0]
        for x in descend_path[1:] + [k]:
            if x.startswith('['):
                tooltip += x
            else:
                tooltip += '.' + x
        tooltip = 'title="%s | %s"' % (
            tooltip,
            typ.__name__)
    else:
        tooltip = ''
    return ('<tr><td %s valign=top>%s<br/><span class="type">%s</span>' +
            '</td><td valign=top>%s</td>') % (
        tooltip,
        k,
        typ.__name__,
        v)

def enumerate_seq(seq, sorted_keys=None):
    if isinstance(seq, list):
        for idx, val in enumerate(seq):
            yield '[%s]' % idx, val
    elif isinstance(seq, (feedparser.FeedParserDict, dict)):
        for attr in (sorted_keys or sorted(seq.keys())):
            try:
                val = getattr(seq, attr)
            except AttributeError:
                val = seq[attr]
            yield attr, val


def htmlize(obj, sorted_keys=None, descend_path=[]):
    if isinstance(obj, (feedparser.FeedParserDict, dict, list)):
        res = []
        for attr, val in enumerate_seq(obj, sorted_keys):
            res.append(tr(attr,
                          htmlize(val, descend_path=descend_path+[attr]),
                          type(val),
                          descend_path))
        return table % '\n'.join(res)
    else:
        #if isinstance(obj, unicode):
        #    obj = obj.encode('ascii', 'xmlcharrefreplace')
        escaped = cgi.escape(repr(obj))
        wrapped = textwrap.wrap(escaped)
        link = ''
        if isinstance(obj, (unicode, str)):
            if obj.startswith('http://') or obj.startswith('https://'):
                if len(wrapped) < 200:
                    link = ' <a href="%s">[link]</a>' % obj
        return '\n<br/>'.join(wrapped) + link

def main(url):
    f = feedparser.parse(url)
    keys = sorted(f.keys())
    keys.remove('entries')
    keys.append('entries') # place it last
    sub = htmlize(f, keys, ['f'])
    if f.bozo:
        sub = bozo(str(f.bozo_exception)) + sub
    h = html % sub
    return h

if __name__ == '__main__':
    h = main('http://defcraft.blogspot.com/feeds/posts/default')
    open('test.html', 'w').write(h)
