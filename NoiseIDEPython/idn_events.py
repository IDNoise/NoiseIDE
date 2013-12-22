__author__ = 'IDNoise'


class Event:
    def __init__(self):
        self.handlers = []

    def __add__(self, handler):
        if not handler is self.handlers:
            self.handlers.append(handler)
        return self

    def __sub__(self, handler):
        if handler is self.handlers:
            self.handlers.remove(handler)
        return self

    def __call__(self, *args, **kwargs):
        for handler in self.handlers:
            handler(*args, **kwargs)