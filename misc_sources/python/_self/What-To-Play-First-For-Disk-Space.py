#!/usr/bin/env python
"""
What-To-Play-First-For-Disk-Space
Shows sorted mediafiles with ratio of file-size to play-time.
"""

import os
import pprint
import string
import subprocess
import sys


def playtime_in_seconds(mediafile):
    cmd = "ffprobe -show_entries format=duration -v quiet -of csv=\"p=0\" -i \"%s\"" % (mediafile)
    return subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE).stdout.read()


def file_size_in_bytes(mediafile):
    return os.path.getsize(mediafile)


def filetype_is_media(filepath):
    if filepath.lower().endswith(('.mp4', '.mkv', '.flv', '.avi', '.mpg', '.mpeg', '.ogv')):
        return True
    if filepath.lower().endswith(('mp3', 'flac', 'ogg')):
        return True
    return False


def mediafiles_spec(path, files):
    mediafiles = []
    for file in files:
        spec = { "filename": os.path.join(path, file) }
        try:
            spec["length"] = string.atof(playtime_in_seconds(spec["filename"]))
        except:
            continue
        if not filetype_is_media(file):
            continue
        spec["size"] = file_size_in_bytes(spec["filename"])
        spec["ratio"] = spec["size"] / spec["length"]
        mediafiles.append(spec)
    return mediafiles


def path_walker(dirpath):
    mediafiles = []
    for (path, dirs, files) in os.walk(dirpath):
        mediafiles += mediafiles_spec(path, files)
    return sorted(mediafiles, key=lambda k: k['ratio'], reverse=True)


def mediafiles_to_stdout(mediafiles, filename):
    for _f in mediafiles:
        pprint.pprint("%dMB %dmin :: %s" % (_f["size"]/(1024*1024.0),
                                                  _f["length"]/60,
                                                  _f["filename"].split("/")[-1] )
                     )
        pprint.pprint("path: %s" % _f["filename"])


def mediafiles_to_m3u(mediafiles, filename):
    with open(filename, 'w') as m3u:
        for media in mediafiles:
            m3u.write("# %dMB %dmin :: %s\n" % (media["size"]/(1024*1024.0),
                                                media["length"]/60,
                                                media["filename"].split("/")[-1]
                                               )
                     )
            m3u.write("%s\n" % (media["filename"]))


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: %s <Path-To-Media-Dir> <Output-Option>" % (sys.argv[0]))
        sys.exit(1)
    mediafiles = path_walker(sys.argv[1])
    if sys.argv[2].lower().endswith('.m3u'):
        mediafiles_to_m3u(mediafiles, sys.argv[2])
    else:
        mediafiles_to_stdout(mediafiles)
