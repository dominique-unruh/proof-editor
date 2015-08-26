import logging, sys
import logging.config

logging.config.dictConfig({
    'version': 1, 
    'root': {'handlers': ['default'], 'level':'DEBUG'}, 
    'handlers':{
        'default': {
            'class':  'logging.StreamHandler', 
            'level': 'DEBUG',
            'stream': sys.stdout, 
            'formatter':'default'
        }
    }, 
    'formatters':{
        'default': {
            'format': '%(filename)s:%(lineno)s (%(funcName)s): %(message)s (%(levelname)s)', 
        }
    }
}) # type: ignore

dummy = 0

logging.info("Logging initialized")
