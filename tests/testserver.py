import os

from flask import (
    Flask, request, session, redirect, abort, jsonify)
from werkzeug.http import HTTP_STATUS_CODES

app = Flask(__name__)
app.secret_key = 'SECRET-KEY-FOR-EMACS-REQUEST-DEVELOPMENT'

all_methods = ['GET', 'POST', 'PUT', 'DELETE']


# View functions


@app.route('/report/<path:path>', methods=all_methods)
def page_report(path):
    """
    Report back path, input data, parameter, etc. as JSON.
    """
    return jsonify(dict(
        path=path,
        data=request.data,
        form=request.form,
        args=request.args,
        cookies=request.cookies,
        method=request.method,
        json=request.json,
        username=session.get('username'),
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


@app.route('/login', methods=['GET', 'POST'])
def page_login():
    error = 'Not logged-in'
    if request.method == 'POST':
        username = request.form['username']
        if 'invalid' in username:
            error = 'Invalid username'
        elif 'invalid' in request.form['password']:
            error = 'Invalid password'
        else:
            session['username'] = username
            return redirect('report/from-login')
    return error


@app.route('/logout')
def page_logout():
    session.pop('username', None)
    return redirect('report/from-logout')


# Runner


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
