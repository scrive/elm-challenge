# Scrive elm challenge

Hello! Welcome to the Scrive Elm Challenge.

This repository contains requirements definition of a project used for job applications
on a **position of an [Elm](https://elm-lang.org/) developer in Scrive**.
The whole assignment is based on a real part of our production app and real needs we had addressed.

It also contains the skeleton of an Elm app with technologies we are currently 
using - [Tailwind CSS](https://tailwindcss.com) and [Vite](https://vitejs.dev).

## Goal

The goal is to test your ability to come up with a solution for a real world problem which will be part of your day to day responsibility.
Obviously the first thing what we will look at is a degree to what your implementation satisfies original requirements.
Also we want to see your ability to come up with a robust solution and will look at the overall code quality.

## Where to Start

If you are interested in applying for this position or just want to challenge yourself (which is also 100% OK with us),
please continue following the steps:

- Fork this repository under your GitHub account.
- Complete an implementation inside your fork.
- Open pull request to [original](https://github.com/scrive/elm-challenge/) repository with your own implementation.
- Make your pull request ready for review to let us know that we can review your code.
- Comment your PR with any question in case you need any help (or send us email - see below).

**You can also open a draft pull request before you finish the implementation in case you want to discuss anything with us!**

---

# Instructions

Your task is provide an implementation of an edit form for User group data.

You have to deliver at least one part of your choice. (You can do more parts of course) :

- Settings
- Contact details
- Tags

Feel free to use any package or technique you are used to or you would like to try out.

You have a data provided to you as a `JSON` string in `Data.elm` module. They are based on what our [API](https://apidocs.scrive.com/#view-user-group) returns with small addition in Contacts for this task.

## Criteria

You should create a form for the part of your choice. 

- The form should load the data from `Data.elm` module.
- It should validate according to the rules described in the data description for your part of choice as below

Nice to have for the form:

- it is styled to follow nice UX/UI design
- it is responsive
- it is accessible

## Data description
Data should be easy to follow and understand. The main three interesting parts are Settings, Contact details and Tags

### Settings

This part of data controls how many days can the document stay in certain state (preparation, closed, canceled, timedout, rejected, error) before it is discarded. Value of `null` means that it is never discarded in this state.

These settings can be also inherited from any user group above.

### Contact details

This part of data contains details on the contacts for user group. Quite simple with one caveat (which was added just for the sake of this task, it is not part of our official API).

User group can have one of these means of preferred contact - `post`, `email`, `phone`. 

These restrictions are in play:
  - if `post` is selected then the address is mandatory to be filled
  - if `email` is selected, then the email is mandatory to be filled
  - if `phone` is selected then the phone is mandatory to be filled

### Tags

Simple list of `name` and `value` of tags. There should not be duplicate tags allowed.

### Inheritance

User groups live in a tree structure - they can have many children User Groups which can inherit Settings and Contact details. 
User group with `parent_id: null` is the root user group and cannot inherit anything.



