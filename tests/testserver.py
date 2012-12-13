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

    method = environ['REQUEST_METHOD']
    params = {}
    input = None
    wsgi_input = environ['wsgi.input']

    size = environ['CONTENT_LENGTH']
    if size:
        if method == 'POST':
            fs = cgi.FieldStorage(environ=environ, fp=wsgi_input)
            params = dict((k, fs.getlist(k)) for k in fs.keys())
        else:
            input = wsgi_input.read(int(size))

    start_response(
        '200 OK', [('Content-Type', 'application/json')])

    return json.dumps(dict(
        path=environ['PATH_INFO'],
        input=input,
        params=params,
        method=method,
    ))


def run(port):
    import sys
    from wsgiref.simple_server import make_server

    port = port or get_open_port()
    print port
    sys.stdout.flush()

    server = make_server('', port, testingapp)
    server.serve_forever()


def main(args=None):
    import argparse
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--port', default=0, type=int)
    ns = parser.parse_args(args)
    run(**vars(ns))


if __name__ == '__main__':
    main()
