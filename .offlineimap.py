#!/usr/bin/python2
import subprocess
import os

def mailpasswd( acct ):
    acct = os.path.basename( acct )
    path = "/home/sjarvis/.pass/%s.pass.gpg" %acct
    args = [ "gpg", "--quiet", "--batch", "-d", path ]
    try:
        return subprocess.check_output( args ).strip()
    except:
        return "not found"
