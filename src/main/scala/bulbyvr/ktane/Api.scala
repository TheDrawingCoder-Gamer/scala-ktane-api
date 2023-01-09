package bulbyvr.ktane


import java.io.* 
import scala.jdk.StreamConverters.* 
import java.util.Date
import java.time.LocalDate
import io.circe.*
import io.circe.Decoder.Result
import io.circe.syntax.*
import cats.implicits.* 
import java.time.format.DateTimeFormatter
def loadFromJSON(json: Json): Option[KtaneMod] = {
  json.as[KtaneMod].toOption
}


val jsonDateFormat = java.text.SimpleDateFormat("yyyy-mm-dd")
val publishDateFormat = DateTimeFormatter.ofPattern("yyyy-mm-dd")
enum BossStatus {
  case FullBoss, SemiBoss, NotBoss
}

given Decoder[BossStatus] = c => 
  for {
    s <- c.as[String]
    r <- 
      s match {
        case "FullBoss" => Right(BossStatus.FullBoss)
        case "SemiBoss" => Right(BossStatus.SemiBoss)
        case "NotBoss" => Right(BossStatus.NotBoss) 
        case it => Left(DecodingFailure(s"Expected FullBoss, SemiBoss, or NotBoss. Got $it instead.", c.history))
      }
  } yield r
given Encoder[BossStatus] = bs => 
  bs match
    case BossStatus.FullBoss => "FullBoss".asJson 
    case BossStatus.SemiBoss => "SemiBoss".asJson 
    case BossStatus.NotBoss => "NotBoss".asJson 
  

enum Compatibility {
  case Compatible, Problematic, Unplayable
}
given Decoder[Compatibility] = c => 
  for {
    s <- c.as[String]
    r <- 
      s match {
        case "Compatible" => Right(Compatibility.Compatible)
        case "Problematic" => Right(Compatibility.Problematic)
        case "Unplayable" => Right(Compatibility.Unplayable)
        case it => Left(DecodingFailure(s"Failure while parsing Compatiblity. Got $it.", c.history))
      }
  } yield r
given Encoder[Compatibility] = c => 
  c match
    case Compatibility.Compatible => "Compatible".asJson 
    case Compatibility.Problematic => "Problematic".asJson 
    case Compatibility.Unplayable => "Unplayable".asJson 
  
object Compatibility {
  def fromOption(opt: Option[String]): Option[Compatibility] = {
    opt match {
      case Some(it) => parse(it)
      case None => Some(Compatible)
    }
  }
  def parse(str: String): Option[Compatibility] = 
    str match {
      case "Compatible" => Some(Compatible)
      case "Problematic" => Some(Problematic)
      case "Unplayable" => Some(Unplayable) 
      case _ => None 
    }
}
enum Difficulty {
  case VeryEasy
  case Easy
  case Medium
  case Hard
  case VeryHard
}
object Difficulty {
  def parse(str: String): Option[Difficulty] = 
    str match {
      case "VeryEasy" => Some(VeryEasy)
      case "Easy" => Some(Easy)
      case "Medium" => Some(Medium)
      case "Hard" => Some(Hard)
      case "VeryHard" => Some(VeryHard)
      case _ => None 
    }
}

given Decoder[Difficulty] = c => 
  for {
    s <- c.as[String] 
    r <- 
      s match {
        case "VeryEasy" => Right(Difficulty.VeryEasy)
        case "Easy" => Right(Difficulty.Easy)
        case "Medium" => Right(Difficulty.Medium)
        case "Hard" => Right(Difficulty.Hard)
        case "VeryHard" => Right(Difficulty.VeryHard)
        case it => Left(DecodingFailure(s"Error while parsing Difficulty. Got $it.", c.history))
      }
  } yield r
given Encoder[Difficulty] = d => 
  d match
    case Difficulty.VeryEasy => "VeryEasy".asJson 
    case Difficulty.Easy => "Easy".asJson 
    case Difficulty.Medium => "Medium".asJson 
    case Difficulty.Hard => "Hard".asJson 
    case Difficulty.VeryHard => "VeryHard".asJson 
  
enum SouvenirStatus {
  case Supported
  case NotACandidate
  case Candidate(reason: Option[String])
  case Unexamined
}
given Decoder[SouvenirStatus] with {
  def apply(c: HCursor): Result[SouvenirStatus] = {
    val status = c.downField("Status").as[String].getOrElse("")
    val good = 
      status match {
        case "Supported" => SouvenirStatus.Supported 
        case "NotACandidate" => SouvenirStatus.NotACandidate 
        case "Candidate" => SouvenirStatus.Candidate(None)
        case "" => 
          c.downField("Explanation").as[String].map(it => SouvenirStatus.Candidate(Some(it))).getOrElse(SouvenirStatus.Unexamined)
        case _ => SouvenirStatus.Unexamined
      } 
    Right(good)
  }
}

given Encoder[SouvenirStatus] with {
  def apply(a: SouvenirStatus): Json = 
    a match
      case SouvenirStatus.Supported => JsonObject("Status" -> "Supported".asJson).asJson
      case SouvenirStatus.NotACandidate => JsonObject("Status" -> "NotACandidate".asJson).asJson
      case SouvenirStatus.Candidate(None) =>
        JsonObject("Status" -> "Candidate".asJson).asJson 
      case SouvenirStatus.Candidate(Some(reason)) => JsonObject("Explanation" -> reason.asJson).asJson 
      case SouvenirStatus.Unexamined => JsonObject("Status" -> "Unexamined".asJson).asJson
    
}
enum ModKind {
  case Regular, Needy, Widget, Holdable
  case Other(name: String)

  def asString: String = 
    this match
      case ModKind.Regular => "Regular"
      case ModKind.Needy => "Needy"
      case ModKind.Widget => "Widget"
      case ModKind.Holdable => "Holdable"
      case ModKind.Other(name) => name 
    
}
object ModKind {
  def parse(str: String): ModKind = {
    str match {
      case "Regular" => Regular 
      case "Needy" => Needy 
      case "Widget" => Widget 
      case "Holdable" => Holdable 
      case _ => Other(str)
    }
  }
}

given Decoder[ModKind] = c => 
  for {
    s <- c.as[String]
  } yield ModKind.parse(s)
given Encoder[ModKind] = _.asString.asJson
enum Quirk {
  case SolvesAtEnd
  case TimeDependent
  case NeedsOtherSolves 
  case SolvesBeforeSome
  case WillSolveSuddenly
  case PseudoNeedy 
  case NeedsImmediateAttention
  case SolvesWithOthers

  def asString: String = 
    this match
      case Quirk.SolvesAtEnd => "SolvesAtEnd"
      case Quirk.TimeDependent => "TimeDependent"
      case Quirk.NeedsOtherSolves => "NeedsOtherSolves"
      case Quirk.SolvesBeforeSome => "SolvesBeforeSome"
      case Quirk.WillSolveSuddenly => "WillSolveSuddenly"
      case Quirk.PseudoNeedy => "PseudoNeedy"
      case Quirk.NeedsImmediateAttention => "NeedsImmediateAttention"
      case Quirk.SolvesWithOthers => "SolvesWithOthers"
    
}
object Quirk {
  def parse(str: String): Option[Quirk] = {
    str match {
      case "SolvesAtEnd" => Some(SolvesAtEnd)
      case "TimeDependent" => Some(TimeDependent)
      case "NeedsOtherSolves" => Some(NeedsOtherSolves)
      case "SolvesBeforeSome" => Some(SolvesBeforeSome)
      case "WillSolveSuddenly" => Some(WillSolveSuddenly)
      case "PseudoNeedy" => Some(PseudoNeedy)
      case "NeedsImmediateAttention" => Some(NeedsImmediateAttention)
      case "SolvesWithOthers" => Some(SolvesWithOthers)
      case _ => None
    }
  }
}
given Decoder[Quirk] = c => 
  for {
    s <- c.as[String]
    r <- Quirk.parse(s).toRight(DecodingFailure(s"Error parsing Quirk. Got $s.", c.history))
  } yield r
given Encoder[Quirk] = _.asString.asJson

val quirkStrArrayDec: Decoder[Vector[Quirk]] = c => {
  for {
    s <- c.as[String]
    r <- s.split(",").map(_.trim).map(Quirk.parse).toVector.sequence.toRight(DecodingFailure("Error while parsing quirks.", c.history))
  } yield r
}
val quirkStrArrayEnc: Encoder[Vector[Quirk]] = qs => {
  qs.map(_.asString).foldLeft("")(_ + ", " + _).asJson
}
case class ModuleAbout(
    contributors: Contributors,
    author: String,
    moduleId: String, 
    desc: String, 
    name: String 
  )
case class Vec2i(x: Int, y: Int)
case class KtaneMod(
    bossStatus: BossStatus, 
    compat: Compatibility,
    defuserDifficulty: Difficulty,
    expertDifficulty: Difficulty,
    modId: Option[String],
    about: ModuleAbout,
    published: LocalDate,
    supportsRuleSeed: Boolean, 
    timeMode: Option[TimeMode],
    souvenirStatus: SouvenirStatus, 
    symbol: String, 
    tutorialVideos: Vector[TutorialVideo],
    kind: ModKind,
    twitchPlays: Option[TwitchPlays], 
    sheets: Vector[String],
    pos: Vec2i,
    sourceUrl: Option[String],
    quirks: Vector[Quirk]
  )

given Decoder[KtaneMod] = c => 
  for {
    bossStatus <- c.getOrElse[BossStatus]("BossStatus")(BossStatus.NotBoss)
    compat <- c.getOrElse[Compatibility]("Compatibility")(Compatibility.Compatible)
    defuserDiff <- c.get[Difficulty]("DefuserDifficulty")
    expertDiff <- c.get[Difficulty]("ExpertDifficulty")
    steamId <- c.getOrElse[Option[String]]("SteamID")(None)
    contributors <- c.getOrElse[Contributors]("Contributors")(Contributors(Map()))
    author <- c.get[String]("Author")
    moduleID <- c.get[String]("ModuleID")
    desc <- c.get[String]("Description")
    name <- c.get[String]("Name")
    about = ModuleAbout(contributors, author, moduleID, desc, name)
    published <- c.get[LocalDate]("Published")
    supportsRuleSeed <- c.getOrElse[String]("RuleSeedSupport")("").map(_ == "Supported")
    timeMode <- c.getOrElse[Option[TimeMode]]("TimeMode")(None)
    souvineerStatus <- c.getOrElse[SouvenirStatus]("Souvenir")(SouvenirStatus.Unexamined)
    symbol <- c.getOrElse[String]("Symbol")("??")
    tutVideos <- c.getOrElse[Vector[TutorialVideo]]("TutorialVideos")(Vector())
    kind <- c.get[ModKind]("Type")
    twitchPlays <- c.getOrElse[Option[TwitchPlays]]("TwitchPlays")(None)
    sheets <- c.get[Vector[String]]("Sheets")
    x <- c.get[Int]("X")
    y <- c.get[Int]("Y")
    sourceUrl <- c.getOrElse[Option[String]]("SourceUrl")(None)
    quirks <- c.getOrElse[Vector[Quirk]]("Quirks")(Vector())(quirkStrArrayDec)
  } yield 
    KtaneMod(
      bossStatus, 
      compat, 
      defuserDiff, 
      expertDiff,
      steamId,
      about,
      published,
      supportsRuleSeed,
      timeMode,
      souvineerStatus,
      symbol,
      tutVideos,
      kind,
      twitchPlays,
      sheets,
      Vec2i(x, y),
      sourceUrl,
      quirks
      )
given Encoder[KtaneMod] = m => 
  JsonObject(
    "BossStatus" -> m.bossStatus.asJson, 
    "Compatibility" -> m.compat.asJson,
    "DefuserDifficulty" -> m.defuserDifficulty.asJson,
    "ExpertDifficulty" -> m.expertDifficulty.asJson,
    "SteamID" -> m.modId.asJson, 
    "Contributors" -> m.about.contributors.asJson,
    "Author" -> m.about.author.asJson,
    "ModuleID" -> m.about.moduleId.asJson,
    "Description" -> m.about.desc.asJson,
    "Name" -> m.about.name.asJson, 
    "Published" -> publishDateFormat.format(m.published).asJson,
    "RuleSeedSupport" -> (if (m.supportsRuleSeed) "Supported".asJson else "NotSupported".asJson),
    "TimeMode" -> m.timeMode.asJson,
    "Souvenir" -> m.souvenirStatus.asJson, 
    "Symbol" -> m.symbol.asJson, 
    "TutorialVideos" -> m.tutorialVideos.asJson,
    "Type" -> m.kind.asJson, 
    "TwitchPlays" -> m.twitchPlays.asJson, 
    "Sheets" -> m.sheets.asJson, 
    "X" -> m.pos.x.asJson, 
    "Y" -> m.pos.y.asJson, 
    "SourceUrl" -> m.sourceUrl.asJson,
    "Quirks" -> m.quirks.asJson(quirkStrArrayEnc)
  ).asJson
case class TutorialVideo(
    language: String,
    url: String
  )

given Decoder[TutorialVideo] = c => 
  for {
    lang <- c.downField("Language").as[String]
    url <- c.downField("Url").as[String]
  } yield TutorialVideo(lang, url)

given Encoder[TutorialVideo] = tv => 
  JsonObject("Language" -> tv.language.asJson, "Url" -> tv.url.asJson).asJson
case class TwitchPlays (
    score: Int,
    scoreDesc: String
  )
object TwitchPlays {
  def fromJSON(json: JsonObject) = {
    for {
      score <- json("Score").flatMap(_.asNumber).flatMap(_.toInt)
      scoreDesc <- json("ScoreStringDescription").flatMap(_.asString) 
    } yield TwitchPlays(score, scoreDesc)
  }
}
given Decoder[TwitchPlays] = c => 
 for {
    score <- c.downField("Score").as[Int]
    scoreDesc <- c.downField("ScoreStringDescription").as[String]
 } yield TwitchPlays(score, scoreDesc)
given Encoder[TwitchPlays] = tp => 
  JsonObject("Score" -> tp.score.asJson, "ScoreStringDescription" -> tp.scoreDesc.asJson).asJson
case class TimeMode(
    score: Int, 
    origin: String 
  )
object TimeMode {
  def fromJSON(json: JsonObject) = {
    for {
      score <- json("Score").flatMap(_.asNumber).flatMap(_.toInt)
      origin <- json("Origin").flatMap(_.asString)
    } yield TimeMode(score, origin)
  }
}
given Decoder[TimeMode] = c => 
  for {
    score <- c.downField("Score").as[Int]
    origin <- c.downField("Origin").as[String]
  } yield TimeMode(score, origin)
given Encoder[TimeMode] = tm => 
  JsonObject("Score" -> tm.score.asJson, "Origin" -> tm.origin.asJson).asJson
case class Contributors(
    children: Map[String, Vector[String]]
  )
object Contributors {
  def parse(json: JsonObject) = {
    val map = json.toMap
    val rest = map.filter( (k, v) => v.asArray.isDefined).mapValues(_.asArray.get.map(_.asString).flatten)
    Contributors(rest.toMap)
  }
}

given Decoder[Contributors] = c => 
  for {
    obj <- c.as[Map[String, Vector[String]]]
  } yield Contributors(obj)

given Encoder[Contributors] = c => 
  c.children.asJson


