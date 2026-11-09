"""Shared isolated-script import bootstrap; test modules never edit sys.path."""
from pathlib import Path
import sys

directory = str(Path(__file__).absolute().parent)
if directory not in sys.path:
    sys.path.insert(0, directory)
