from flask import (
    Flask, request, jsonify)

app = Flask(__name__)


@app.route('/report/<path:path>', methods=['GET', 'POST', 'PUT'])
def report(path):
    method = request.method
    params = {}
    input = None

    if method == 'POST':
        params = request.form
    else:
        input = request.data

    return jsonify(dict(
        path=path,
        input=input,
        params=params,
        method=method,
    ))


def get_open_port():
    import socket
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.bind(("", 0))
    s.listen(1)
    port = s.getsockname()[1]
    s.close()
    return port


def run(port):
    import sys
    port = port or get_open_port()
    print port
    sys.stdout.flush()
    app.run(port=port, debug=True, use_evalex=False)


def main(args=None):
    import argparse
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--port', default=0, type=int)
    ns = parser.parse_args(args)
    run(**vars(ns))


if __name__ == '__main__':
    main()
