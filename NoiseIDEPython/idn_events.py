__author__ = 'IDNoise'


class Event:
    def __init__(self):
        self.handlers = []

    def __add__(self, handler):
        self.handlers.append(handler)

    def __sub__(self, handler):
        self.handlers.remove(handler)

    def __call__(self, *args, **kwargs):
        for handler in self.handlers:
            handler(*args, **kwargs)