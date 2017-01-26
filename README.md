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
3. Now you can configure some things by changing the source (sorry!). In
   src/{Main,Config}.hs, you can set the timezone, deadline, and report
   schedule.
4. Build `attendancebot` with `make`. The binary ends up in .stack-work/install
   somewhere.
5. You can now start `attendancebot` with the following environment variables
   set appropriately:

        SLACK_API_TOKEN="abcd-12345678901-ABCDEFGHIJKLMNOPQRSTUVWX"
        GOOGLE_APPLICATION_CREDENTIALS="/usr/local/etc/attendancebot/google-credentials.json"
        SPREADSHEET_ID="A1B2C2D-4E5F6G7H8I9J0K1L2M3N4O4P5Q6R7S8T9U0V"
        SPREADSHEET_RANGE="A4:K369"
        ATTENDANCEBOT_USER="U1A2B3C4D"
        ANNOUNCEMENT_CHANNEL="C1A2B3C4D"
        ATTENDANCE_LOG="/var/lib/attendancebot/eventlog"

If you want to use the included systemd service file (attendancebot.service),
then put the above variables in /usr/local/etc/attendancebot/defaults and move
the binary to /usr/local/bin/attendancebot. Also, make sure you add a user
called "attendancebot", and that `ATTENDANCE_LOG` is readable/writable for this
user.

All of this could be a lot smoother, but as I said, this is unreleased
software.

[bot user]: https://api.slack.com/bot-users
[create bot user]: https://my.slack.com/services/new/bot
[google cloud platform]: https://cloud.google.com/
[google cloud auth]: https://cloud.google.com/docs/authentication
