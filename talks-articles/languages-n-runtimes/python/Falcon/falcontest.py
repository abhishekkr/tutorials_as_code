import falcon
import json


class SomeKlass():
    def on_get(self, req, resp):
        """Handles GET requests"""
        result = { "status": "ToBeDone" }
        resp.body = json.dumps(result)

    def on_post(self, req, resp):
        """Handles POST requests"""
        try:
            raw_json = req.stream.read()
        except Exception as ex:
            raise falcon.HTTPError(falcon.HTTP_400,'Error',ex.message)

        try:
            post_body = json.loads(raw_json, encoding='utf-8')
            response = {"what-i-heard-was": str(post_body)}
            resp.body = str(response)
        except ValueError as vex:
            raise falcon.HTTPError(falcon.HTTP_400,
                                   'Invalid JSON',
                                   "Could not decode the request body. JSON was incorrect. %s." % (vex))

api = falcon.API()
api.add_route('/tut', SomeKlass())
