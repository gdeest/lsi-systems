module LSI.AccuracyModel where

import qualified Data.IntMap as IM

type AccuracyModel d = IM.IntMap (d, d)


-- buildAccuracyModel ::
