#!/usr/bin/env python
"""
Reference: https://www.youtube.com/watch?v=L6hQCgxgzLI

* Create or select a project in the Google Developers Console and enable the API.
> https://console.developers.google.com//start/api?id=gmail&credential=client_key

* Under 'Credentials', create 'Oauth2', for 'Installed Applications', download Client Secret JSON

The downloaded JSON is to be provided as param to this script.


It's not the exact script shown in the Google Developer Video,
but the script there was good logic but dirty code,
its just cleaner version of same logic.
"""

import sys
from apiclient.discovery import build
from httplib2 import Http
from oauth2client import file, client, tools


def GMAIL_handler(storage_json, client_secret, scopes):
    store = file.Storage(storage_json)
    creds = store.get()

    if creds is None or creds.invalid:
        flow = client.flow_from_clientsecrets(client_secret, scopes)
        creds  = tools.run(flow, store)

    return build('gmail', 'v1', http=creds.authorize(Http()))


def mail_thread_counts(thread_data):
    if len(thread_data['messages']) <= 2:
        return
    msg = thread_data['messages'][0]['payload']
    for header in msg['headers']:
        if header['name'] == 'Subject':
            return header['value']


def process_threads(gmail):
    mail_threads = gmail.users().threads().list(userId='me').execute().get('threads', [])

    for mail_thread in mail_threads:
        thread_data = gmail.users().threads().get(userId='me', id=mail_thread['id']).execute()
        subject = mail_thread_counts(thread_data)
        if subject:
            print "%s (%s msgs)" % (subject, len(thread_data['messages']))


if __name__ in "__main__":
    scopes = "https://www.googleapis.com/auth/gmail.readonly"
    client_secret = sys.argv[1]
    storage_json = "%s.json" % client_secret
    gmail = GMAIL_handler(storage_json, client_secret, scopes)
    process_threads(gmail)
