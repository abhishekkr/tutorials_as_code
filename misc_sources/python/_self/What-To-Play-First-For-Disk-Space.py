#!/usr/bin/env python
"""
What-To-Play-First-For-Disk-Space

Requirements:
    * Python (https://en.wikipedia.org/wiki/Python_(programming_language))
    * FFMPEG (https://en.wikipedia.org/wiki/FFmpeg)

This utility helps with two problems:
    * Need to empty some disk-space quickly but have lots of videos/podcasts on it yet to finish.
    * Just have too many videos/podcasts to finish and confused in what order.

It prints or let you create a playlist(m3u,pls) for all Media Filetypes (supported by 'filetype_is_media').
The playlist is in order of playing files with largest ratio of Play-Time of file with its disk-space.
Like if we have 2 files: A{size: 10MB, length: 5min} and B{size: 9MB, length: 7min}
The playlist will have File.A listed before File.B.

Syntax:

* Print the order of files to be played on console
```
What-To-Play-First-For-Disk-Space.py <Path-With-Media>
```

* To create M3U Playlist of order of files
```
What-To-Play-First-For-Disk-Space.py <Path-With-Media> <Playlist-Path-With-m3u-extension>
```

* To create PLS Playlist of order of files
```
What-To-Play-First-For-Disk-Space.py <Path-With-Media> <Playlist-Path-With-pls-extension>
```

Dramatic Version:
There comes a time in one's life, when you have too many videos or podcasts to finish and too little HardDisk space left free.
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
    if filepath.lower().endswith(('.mp4', '.mkv', '.flv', '.avi', '.mpg',
                                  '.mpeg', '.ogv', '.vob', '.webm', '.3gp',
                                  '.3gpp', '.wmv', '.m4v', '.m4p', '.mov',
                                  '.rm', '.rmvb', '.asf', '.3g2')):
        return True
    if filepath.lower().endswith(('.wav', '.mp3', '.flac', '.ogg', '.m4a',
                                  '.vox', '.au', '.oga', '.wma', '.wv')):
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
        mediafiles.append(spec)
    return mediafiles


def path_walker(dirpath):
    mediafiles = []
    for (path, dirs, files) in os.walk(dirpath):
        mediafiles += mediafiles_spec(path, files)
    return sorted(mediafiles, key=lambda k: (k['size']/k['length'], k['size']), reverse=True)


def media_title_from_spec(spec):
    return "%dMB %dmin :: %s" % (spec["size"]/(1024*1024.0),
                                 spec["length"]/60,
                                 spec["filename"].split("/")[-1])


def mediafiles_to_stdout(mediafiles):
    for media in mediafiles:
        pprint.pprint(media_title_from_spec(media))
        pprint.pprint("path: %s" % media["filename"])


def mediafiles_to_m3u(mediafiles, filename):
    with open(filename, 'w') as m3u:
        for media in mediafiles:
            m3u.write("# %s\n" % (media_title_from_spec(media)))
            m3u.write("%s\n" % (media["filename"]))


def mediafiles_to_pls(mediafiles, filename):
    with open(filename, 'w') as pls:
        pls.write("[playlist]\n")
        pls.write("NumberOfEntries=%d\n" % (len(mediafiles)))
        pls.write("Version=2\n")
        pls.write("\n")
        _index = 1
        for media in mediafiles:
            pls.write("Title%d=%s\n" % (_index, media_title_from_spec(media)))
            pls.write("File%d=%s\n" % (_index, media["filename"]))
            pls.write("\n")
            _index += 1


if __name__ == "__main__":
    if sys.argv[1] in ["--help", "-help", "help", "-h", "--h"]:
        print(__doc__)
        sys.exit(0)
    if subprocess.call("which ffprobe > /dev/null", shell=True) != 0:
        print("Need FFMPEG(https://en.wikipedia.org/wiki/FFmpeg) insstalled.")
        sys.exit(1)
    if len(sys.argv) < 2:
        print("Usage: %s <Path-To-Media-Dir> <Output-Option>" % (sys.argv[0]))
        sys.exit(1)
    mediapath = sys.argv[1]
    if not os.path.exists(mediapath):
        print("Path (%s) for media-files doesn't exists." % (mediapath))
        sys.exit(1)
    output = "stdout"
    if len(sys.argv) > 2:
        output = sys.argv[2]

    mediafiles = path_walker(mediapath)
    if output.lower().endswith('.m3u'):
        mediafiles_to_m3u(mediafiles, output)
    elif output.lower().endswith('.pls'):
        mediafiles_to_pls(mediafiles, output)
    else:
        mediafiles_to_stdout(mediafiles)
