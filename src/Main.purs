module Main where

import Prelude

import Data.Array as A
import Data.Foldable (class Foldable)
import Data.List (List)
import Data.String.Common (joinWith)
import Effect (Effect)
import Effect.Console (log)
import Node.Buffer.Class (toString) as Buff
import Node.ChildProcess (defaultExecSyncOptions, execSync)
import Node.Encoding (Encoding(..))
import Options.Applicative.Builder (forwardOptions, header, info, metavar, progDesc, strArgument)
import Options.Applicative.Extra (execParser, helper)
import Options.Applicative.Internal.Utils ((<**>))
import Options.Applicative.Types (Parser, many)

α :: forall f. Foldable f => (forall a. f a -> Array a)
α = A.fromFoldable

type Opts = {
  -- The destination branch name
  dest :: String,
  -- Options to be forwarded to git push
  forwards :: List String
}

parseOpts :: Parser Opts
parseOpts = ado
  dest <- strArgument (metavar "<dest name>")
  forwards <- many (strArgument (metavar "<options forwarded to git push>"))
in { dest, forwards }

main :: Effect Unit
main = do
  { dest, forwards } <- execParser $ info (parseOpts <**> helper) (
    header "git push-as"
    <> progDesc "Push a branch to git with a different name."
    <> forwardOptions
  )
  execSync ("git push origin HEAD:refs/heads/" <> dest <> joinWith " " (α forwards)) defaultExecSyncOptions
  >>= Buff.toString UTF8 >>= log
