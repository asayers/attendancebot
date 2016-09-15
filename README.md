Attendancebot is a slack bot which tracks the attendance of team members by
demanding that they "check-in" by talking to it in the morning.

THIS IS UNSUPPORTED SOFTWARE. Attendancebot is intended for my own use. Perhaps
you will find the code useful too. Don't expect any of the things normally
provided by well-maintained open source projects.

That said, if you want to try to run attendancebot, here are some rough
instructions:

1. Attendancebot runs as a custom slack [bot user]. Create one
   [here][create bot user] (you can call it whatever you want) and record the
   API token.
2. The weekly attendance summaries include a graph which must be uploaded
   somewhere. We use [google cloud storage][] (it's essentially S3). Create a
   [google cloud platform] account if you don't have one, and create a project
   for attendancebot with permission to write to cloud storage. Record the
   credentials (in json format). See [here][google cloud auth] for more on how
   authentication works with google cloud platform.
3. Now you can to configure some things by changing the source (sorry!). In
   src/Main.hs, set the value `user_me` to the user ID of your slack bot, and
   set `channel_annouce` to the ID of the  channel you want attendancebot to
   send reports to. You should also set the timezone, deadline, and report
   schedule.
4. Build `attendancebot` with `make`. The binary ends up in .stack-work/install
   somewhere.
5. You can now start `attendancebot` with the following environment variables:

- `SLACK_API_TOKEN` should be the token for the slack bot user, eg.
  `abcd-01234567890-abcdefghijklmnopqrstuvwxyz`
- `GOOGLE_APPLICATION_CREDENTIALS` should be the path to the google credentials
  file, eg. `/usr/local/etc/attendancebot/google-credentials.json`
- `ATTENDANCE_LOG` should be a (writeable) path where attendancebot will keep
  its database, eg. `/var/lib/attendancebot/eventlog`

If you want to use the included systemd service file (attendancebot.service),
then you'll need to change `EnvironmentFile` to point to a script which sets
the above environment variables, and change `ExecStart` to point to the
attendancebot binary.

All of this could be a lot smoother, but as I said, this is unreleased
software.

[bot user]: https://api.slack.com/bot-users
[create bot user]: https://my.slack.com/services/new/bot
[google cloud platform]: https://cloud.google.com/
[google cloud auth]: https://cloud.google.com/docs/authentication

