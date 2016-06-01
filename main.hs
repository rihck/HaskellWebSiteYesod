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

/login LoginR GET POST
/usuario UsuarioR GET POST
/perfil/#UsersId PerfilR GET
/admin AdminR GET
/logout LogoutR GET
|]

instance Yesod Pagina where
    authRoute _ = Just LoginR
    
    isAuthorized LoginR _ = return Authorized
    isAuthorized ErroR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized UsuarioR _ = return Authorized
    isAuthorized AdminR _ = isAdmin
    isAuthorized _ _ = isUser

isUser = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized
    
isAdmin = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized 
        Just _ -> Unauthorized "Acesso Restrito para Administrador"

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
           
formUser :: Form Users
formUser = renderDivs $ Users <$>
           areq textField "Nome: " Nothing <*>
           areq textField "Login: " Nothing <*>
           areq passwordField "Password: " Nothing        

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

getUsuarioR :: Handler Html
getUsuarioR = do
           (widget, enctype) <- generateFormPost formUser
           defaultLayout [whamlet|
                 <form method=post enctype=#{enctype} action=@{UsuarioR}>
                     ^{widget}
                     <input type="submit" value="Enviar">
           |]

postAnimalR :: Handler Html
postAnimalR = do
           ((result, _), _) <- runFormPost formAnimal
           case result of 
               FormSuccess anim -> (runDB $ insert anim) >>= \piid -> redirect (ChecarAnimalR piid)
               _ -> redirect 
               
postUsuarioR :: Handler Html
postUsuarioR = do
           ((result, _), _) <- runFormPost formUser
           case result of 
               FormSuccess user -> (runDB $ insert user) >>= \piid -> redirect (PerfilR piid)
               _ -> redirect ErroR
               
getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

getAdminR :: Handler Html
getAdminR = defaultLayout [whamlet|
    <b><h1><font size="11"> Bem vindo ao Painel Administrativo</font></h1></b>
|]

getLoginR :: Handler Html
getLoginR = do
           (widget, enctype) <- generateFormPost formLogin
           defaultLayout [whamlet|
                 <form method=post enctype=#{enctype} action=@{LoginR}>
                     ^{widget}
                     <input type="submit" value="Login">
           |]
           
postLoginR :: Handler Html
postLoginR = do
           ((result, _), _) <- runFormPost formLogin
           case result of 
               FormSuccess ("admin","admin") -> setSession "_ID" "admin" >> redirect AdminR
               FormSuccess (login,senha) -> do 
                   user <- runDB $ selectFirst [UsersLogin ==. login, UsersSenha ==. senha] []
                   case user of
                       Nothing -> redirect LoginR
                       Just (Entity pid u) -> setSession "_ID" (pack $ show $ fromSqlKey pid) >> redirect (PerfilR pid)

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
    <h1>Falha no Cadastro !</h1>
|]

connStr = "dbname=d4673as0stmsm7 host=ec2-54-221-225-242.compute-1.amazonaws.com user=nzjfptmglfomng password=fyYms4A9T8gkP4_Go8GswcfIiE port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool)
