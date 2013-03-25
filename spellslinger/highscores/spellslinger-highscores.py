#!/usr/bin/python

from __future__ import print_function

from flask import Flask, Response, request, json
app = Flask(__name__)

import sqlite3, os, logging

DB_NAME = "spellslinger-highscores.db"

#####################
# Web interface
#####################

@app.route("/spellslinger/add-score/<name>/<colour>/<int:score>", methods=["PUT"])
def addScore(name, colour, score):
    with sqlite3.connect(DB_NAME) as db:
        db.execute("INSERT INTO scores VALUES (?, ?, ?)", [name, colour, score])
        db.commit()
    return "ok"

@app.route("/spellslinger/scores")
def scores():
    with sqlite3.connect(DB_NAME) as db:
        vs = db.execute("SELECT name, colour, score "
                        "FROM scores "
                        "ORDER BY score DESC LIMIT 10").fetchall()
        ss = []
        for (name, colour, score) in vs:
            ss.append({"name": name,
                       "colour": colour,
                       "score": score})
        return Response(json.dumps(ss), mimetype="application/json")

#####################
# Main
#####################

def init_db():
    """Initialise the database, if it does not already exist."""
    if not os.path.exists(DB_NAME):
        with sqlite3.connect(DB_NAME) as db:
            db.execute("CREATE TABLE scores ( name TEXT, colour TEXT, score INT ) ")
            db.commit()

def main():
    init_db()

    # Setup logging.
    file_handler = logging.FileHandler("spellslinger-highscores.log")
    log_format = "%(asctime)s %(levelname)s: %(message)s [in %(pathname)s:%(lineno)d]"
    file_handler.setFormatter(logging.Formatter(log_format))
    app.debug_log_format = log_format
    app.logger.setLevel(logging.INFO)
    file_handler.setLevel(logging.INFO)
    app.logger.addHandler(file_handler)

    app.logger.info("spellslinger-highscores starting")
    app.run(debug=False)

if __name__ == "__main__":
    main()
