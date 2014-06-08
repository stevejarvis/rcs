#rcs
====

Originally this was for .rc files, but has become all config and settings stuff.

For most, recommend symlink from ~.

## Notes So I Can Remember What I Did
### Mutt
#### setup
$ echo "password" > ~/.pass/foo.pass
$ gpg --gen-key
$ gpg -r steve.a.jarvis@gmail.com -e ~/.pass/foo.pass
If the foo.pass.gpg file is created all right, kill the plain text.
#### issues
- gpg-agent doesn't seem to always export GPG_AGENT_INFO, leaving the agent and 
passwords unavailable to mutt
- .msmptrc must be 600, so symlink no good

### Vim
- The vundle repo is in the rc file
