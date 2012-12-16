import os

from flask import (
    Flask, request, redirect, abort, jsonify)
from werkzeug.http import HTTP_STATUS_CODES

app = Flask(__name__)

all_methods = ['GET', 'POST', 'PUT', 'DELETE']


@app.route('/report/<path:path>', methods=all_methods)
def page_report(path):
    """
    Report back path, input data, parameter, etc. as JSON.
    """
    return jsonify(dict(
        path=path,
        data=request.data,
        form=request.form,
        method=request.method,
        json=request.json,
    ))


@app.route('/redirect/<path:path>', methods=all_methods)
def page_redirect(path):
    return redirect(path)


@app.route('/code/<int:code>')
def page_code(code):
    try:
        return abort(code)
    except LookupError:
        return HTTP_STATUS_CODES[code], code


def get_open_port():
    import socket
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.bind(("", 0))
    s.listen(1)
    port = s.getsockname()[1]
    s.close()
    return port


def run(port, **kwds):
    import sys
    port = port or get_open_port()
    # Pass port number to child process via envvar.  This is required
    # when using Flask's reloader.
    os.environ['EL_REQUEST_TEST_PORT'] = str(port)
    print port
    sys.stdout.flush()
    app.run(port=port, **kwds)


def main(args=None):
    import argparse
    default_port = int(os.environ.get('EL_REQUEST_TEST_PORT', '0'))
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--port', default=default_port, type=int)
    parser.add_argument('--use-reloader', default=False, action='store_true')
    ns = parser.parse_args(args)
    run(**vars(ns))


if __name__ == '__main__':
    main()
