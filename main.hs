{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Control.Monad.Logger (runStdoutLoggingT)

data Pagina = Pagina{connPool :: ConnectionPool}

instance Yesod Pagina

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Animals json
   nome Text
   idade Int
   deriving Show
   
Users json
   nome Text
   login Text
   senha Text
   deriving Show
|]

mkYesod "Pagina" [parseRoutes|
/ HomeR GET
/animal/cadastro AnimalR GET POST
/animal/checar/#AnimalsId ChecarAnimalR GET
/erro ErroR GET
|]

instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Pagina FormMessage where
    renderMessage _ _ = defaultFormMessage
------------------------

-- Sempre que preciso um form, sera ncessario
-- funcoes deste tipo
formAnimal :: Form Animals
formAnimal = renderDivs $ Animals <$>
           areq textField "Nome: " Nothing <*>
           areq intField "Idade: " Nothing
           

getAnimalR :: Handler Html
getAnimalR = do
           (widget, enctype) <- generateFormPost formAnimal
           defaultLayout $ do 
           toWidget [cassius|
               label
                   color:blue;
           |]
           [whamlet|
                 <form method=post enctype=#{enctype} action=@{AnimalR}>
                     ^{widget}
                     <input type="submit" value="Cadastrar Animal">
           |]

postAnimalR :: Handler Html
postAnimalR = do
           ((result, _), _) <- runFormPost formAnimal
           case result of 
               FormSuccess anim -> (runDB $ insert anim) >>= \piid -> redirect (ChecarAnimalR piid)
               _ -> redirect ErroR
           
getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

getChecarAnimalR :: AnimalsId -> Handler Html
getChecarAnimalR pid = do
    animal <- runDB $ get404 pid
    defaultLayout [whamlet|
    <font size="10">Perfil do Pet</font><br>
        <p><b> Nome do Pet:</b>  #{animalsNome animal}  
        <p><b> Idade do Pet:</b> #{show $ animalsIdade animal} Anos
    |]

getErroR :: Handler Html
getErroR = defaultLayout [whamlet|
    Falha no Cadastro !
|]

connStr = "dbname=d4673as0stmsm7 host=ec2-54-221-225-242.compute-1.amazonaws.com user=nzjfptmglfomng password=fyYms4A9T8gkP4_Go8GswcfIiE port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool)
