attendancebot
=============

A slack bot which demands that team members "check-in" by talking to it in the
morning.

**This is unsupported software.** The  but please
don't expect too much. Attendancebot is intended for internal use.

That said, if you want to try to run attendancebot, here are some rough
instructions:

Prep work
---------

1. Attendancebot runs as a custom [slack bot user]. Create one [here][create
   bot user]. You can call it whatever you want; record the API token.
2. Attendancebot uses various google APIs. Create a [google cloud platform]
   account if you don't have one, and create a project for attendancebot with
   permission to write to cloud storage. Record the credentials in json format.
   ([More info][google cloud auth])
3. The weekly attendance summaries include a graph which must be uploaded
   somewhere accessible. We use google cloud storage, which is like S3. Create
   a bucket for attendancebot to upload these graphs to.
3. We also use the Google account to access an "attendance spreadsheet", which
   team members are meant to use to take holiday. Create this and make a note
   of its spreadsheet id. See src/SpreadSheet.hs for the required format.
4. Now you need to configure some things by changing the source (sorry!). In
   src/Config.hs you can set the deadline, and in src/Schedule.hs you can
   change when the bot performs certain tasks.
5. `attendancebot` logs events to disk so it can be restarted without losing
   data. Choose a place for the log file to live (I use
   /var/lib/attendancebot/eventlog).
6. You also need to choose a channel for `attendancebot` to make announcements
   on. Invite the bot user to the channel, and make a note of the channel id.

[slack bot user]: https://api.slack.com/bot-users
[create bot user]: https://my.slack.com/services/new/bot
[google cloud platform]: https://cloud.google.com/
[google cloud auth]: https://cloud.google.com/docs/authentication

Installing
----------

`attendancebot` is written in Haskell. You need to have [stack] installed.

```
make
sudo make install
```

`make install` puts the binary in /usr/local/bin/ and the service unit file in
/etc/systemd/system/. If you don't want to use systemd, you'll find the binary
in .stack-work/install somewhere after running `make`.

[stack]: www.haskellstack.org

Running
-------

`attendancebot` looks for the following environment variables:

    SLACK_API_TOKEN="abcd-12345678901-ABCDEFGHIJKLMNOPQRSTUVWX"
    GOOGLE_APPLICATION_CREDENTIALS="/usr/local/etc/attendancebot/google-credentials.json"
    SPREADSHEET_ID="A1B2C2D-4E5F6G7H8I9J0K1L2M3N4O4P5Q6R7S8T9U0V"
    SPREADSHEET_RANGE="A4:K369"
    ATTENDANCEBOT_USER="U1A2B3C4D"
    ANNOUNCEMENT_CHANNEL="C1A2B3C4D"
    ATTENDANCE_LOG="/var/lib/attendancebot/eventlog"
    UPLOAD_BUCKET="attendancebot-123456.appspot.com"

If you're using the included systemd service file, put the above variables in
/usr/local/etc/attendancebot/defaults. Also, make sure you add a user called
"attendancebot", and that `ATTENDANCE_LOG` is readable/writable by this user.

Usage
-----

Tracking is opt-in. Team members must register by sending `attendancebot` a
direct message. Thereafter it will ask them to check-in every morning by
talking to it, and it will record the time they do so. If a user has
marked themselves as on-holiday (using the shared spreadsheet) then
`attendancebot` won't bug them.

Every day, `attendancebot` publishes a daily report announcing the day's
late-comers and absentees. If your team has a manager-type person, perhaps
`ANNOUNCEMENT_CHANNEL` should be set to their direct message channel.
Otherwise, you can set it to a regular channel and rely on public shaming to
get results.

> steve still hasn't checked in. bob and alan are on holiday today.

It also publishes a weekly summary to the same channel which includes a matrix
of people are days, specifying whether people were on time, late, on holiday,
or AWOL. It also has a nice chart.

```
steve      ◆◆◆◆◆ | 7.8:1 | 81.9%
alan       ◈◈◈◆◈ | 4.3:1 | 72.9%
bob        ◆◇◆◆◇ | 3.4:1 | 48.1%
phil       HH◆◆◆ | 1.7:1 | 54.3%
Summary for the week beginning 2017-01-16.
Key: daily attendance | on-time:late ratio | score weighted by recency
```

If you say "debug" to attendancebot, it will reply with a dump of its internal
state (or at least, the parts which are likely still to be relevant). If team
members aren't supposed to be able to find out what attendancebot knows, you
might consider this a security issue. Example output:

```
Deadline: 08:45:00
Current time: 2017-01-26 18:59:08.79992

[Check-ins for 2016-08-26]
alan at 08:45:09 (late)
phil at 08:45:24 (late)

[Check-ins for 2016-08-29]
steve at 05:00:52 (on time)
alan at 08:45:06 (late)
phil at 08:45:08 (late)

[Upcoming holidays]
alan on 2017-01-30
bob from 2017-03-06 to 2017-03-09
steve from 2017-02-06 to 2017-02-10

[Scheduled jobs]
CronSchedule 30 23 * * 0-4: remind absent people
CronSchedule 45 23 * * 0-4: send daily summary
CronSchedule 31 3 * * 5: send weekly summary
CronSchedule 0 20 * * 0-4: download spreadsheet
```
