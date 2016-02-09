try:
    from setuptools import setup
except:
    from distutils.core import setup


dependencies = ["docopt", "termcolor"]


setup(
    name="racks",
    version="0.0.1",
    descriptions="racks a test package for stack graphs",
    url="https://github.com/abhishekkr/tutorials_as_code",
    author="AbhishekKr",
    author_email="abhikumar163@gmail.com",
    install_requires=dependencies,
    packages=["racks"], # how many packages app actually contains
    entry_point={   # what's get run when racks get called from comman line
                 "console_script":[
                     'racks=racks.main:start'
                 ],
    },
    classifiers=(
        'Development Status :: 1 - Beta',
        'Intended Audience :: Developers',
        'Natural Language :: English',
        'License :: OSI Approved :: MIT License',
        'Programming Language :: Python',
        'Programming Language :: Python :: 2.6',
        'Programming Language :: Python :: 2.7',
    ),
)


