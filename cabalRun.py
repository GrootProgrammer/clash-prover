import os
import subprocess
from typing import List

subprocess.run(['cabal', 'build'], stderr=open('output.txt', 'w'))