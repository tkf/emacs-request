def get_open_port():
    import socket
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.bind(("", 0))
    s.listen(1)
    port = s.getsockname()[1]
    s.close()
    return port


def testingapp(environ, start_response):
    import json
    import cgi

    wsgi_input = environ['wsgi.input']
    if wsgi_input:
        fs = cgi.FieldStorage(environ=environ, fp=wsgi_input)
        params = dict((k, fs.getlist(k)) for k in fs.keys())
    else:
        params = {}

    start_response(
        '200 OK', [('Content-Type', 'application/json')])
    return json.dumps(dict(
        path=environ['PATH_INFO'],
        params=params,
        method=environ['REQUEST_METHOD'],
    ))


def main():
    import sys
    from wsgiref.simple_server import make_server

    port = get_open_port()
    print port
    sys.stdout.flush()

    server = make_server('', port, testingapp)
    server.serve_forever()


if __name__ == '__main__':
    main()
