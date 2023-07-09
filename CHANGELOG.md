# Revision history for yesod-static-streamly

## 0.1.0.0 -- 2023-07-03

* Released on an unsuspecting world.

## 0.1.1.0 -- 2023-07-03

* Added 0.1.0.0 release date.
* Updated path of Yesod.Static.Streamly to Yesod.Default.Util.Streamly.
* Updated base from 4.16.4.0 (GHC 9.2.7) to 4.17.1.0 (GHC 9.4.5).
* Updated remaining required package version to fit new base.
* Updated documentation for Yesod.Default.Util.Streamly.

## 0.1.1.1 -- 2023-07-03

* First non-candidate version (version bump).

## 0.1.2.1 -- 2023-07-07

* Added staticStreamly function in Yesod.Static.Streamly.
* Added supporting functionality for staticStreamly in Yesod.Static.Streamly.Internal.

## 0.1.3.1 -- 2023-07-07

* Added staticFilesStreamly function to Yesod.Static.Streamly.
* Added supporting functionality for staticFilesStreamly in Yesod.Static.Streamly.Internal.

## 0.1.3.2 -- 2023-07-07

* Updated bounds for template-haskell dependency.

## 0.1.3.3 -- 2023-07-07

* Rework of the sinkHashStreamly and hashFileStreamly functions in Yesod.Static.Streamly.Internal.

## 0.1.3.4 -- 2023-07-08

* Modified hashFileStreamly to set unlimited buffer, threading and eager scheduling for parEval function.

## 0.1.3.5 -- 2023-07-08

* Modified hashFileStreamly to set unlimited buffer (threading and scheduling have no effect on parEval function).
* Modified hashFileStreamly to take in buffer size so that users may optimize chunkReaderWith function based on L2 cache.
