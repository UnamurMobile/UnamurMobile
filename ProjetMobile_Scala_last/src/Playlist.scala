import java.io.File
import java.io.FileInputStream
import java.io.FileNotFoundException
import java.io.FilenameFilter
import java.io.IOException
import java.util.ArrayList
import sun.audio._
import javax.sound._

import javax.sound.sampled.UnsupportedAudioFileException
import com.phidgets.InterfaceKitPhidget
import com.phidgets.PhidgetException
import com.phidgets.event._

import scala.collection.JavaConversions._
import scala.util.Random
import java.io.BufferedInputStream;
import java.io.FileInputStream;
import org.tritonus.share.sampled.file.TAudioFileFormat;
import javazoom.jl.player.Player;
import javazoom.spi.mpeg.sampled.file.MpegAudioFileReader;
//objet principal
object Main{
  //main principal
    def main(args:Array[String]){
    val thread = new Poids_Presence();
        thread.init();
        val t= new Thread(thread)
          t.start()
    
  }
    //class servant a gerer la playlist
class Playlist extends Thread {


  private var playList: ArrayList[Song] = _
//contient la taille du playlist
  private var size: Int = 0

  private var cursor: Int = _

  
  override def run() {
    while (true) PlaySong()
  }

  def PlaySong() {
    try {
      initPlayList()
      for (s <- playList) {
        println(s)
      }
      play()
    } catch {
      case e: UnsupportedAudioFileException => e.printStackTrace()
      case e: IOException => e.printStackTrace()
    }
  }

  def playWav(file: File) {
    try {
      val in = new FileInputStream(file)
      val audio = new AudioStream(in)
      AudioPlayer.player.start(audio)
    } catch {
      case e: IOException => e.printStackTrace()
    }
  }
 def playMp3(file:File){
   val in =new FileInputStream(file.getAbsoluteFile)
       val bis = new BufferedInputStream(in);
       val  player = new Player(bis);
       player.play()
 }
 //permet d'obtenir la longeur du fichier mp3
 def getDurationWithMp3(f :File ):Long={
   
   
   val baseFileFormat = new MpegAudioFileReader().getAudioFileFormat(f);
    	val properties = baseFileFormat.properties();
    	val duration = (properties.get("duration")).asInstanceOf[Long];
    	duration/1000000
 }
  //joue la musique tant que le cursor actuel est inferieur a au nombre de music 
  def play() {
    while (cursor < size) {
      println(playList.get(cursor))
      try {
       // playWav(new File(playList.get(cursor).getNom))
          playMp3(new File(playList.get(cursor).getNom))
        //defini un wait en fonction de la durée de la music 
        Thread.sleep((playList.get(cursor).getDuree ).toLong)
        //passage au music suivant 
        cursor += 1
      } catch {
        case e: InterruptedException => e.printStackTrace()
      }
    }
    cursor = 0
    play()
  }
// initialisation du playlist
  def initPlayList() {
    playList = new ArrayList[Song]()
    val listName = listFilesNameForFolder(new File("C:/test/"))
    for (name <- listName) {
    
      val file = new File("C:/test/" + name)
       //for Wav File
     //val audioInputStream = AudioSystem.getAudioInputStream(file)
      //val format = audioInputStream.getFormat
      //val frames = audioInputStream.getFrameLength
      //val durationInSeconds = (frames + 0.0) / format.getFrameRate
     // playList.add(new Song("C:/test/" + name, math.floor(durationInSeconds)))
      playList.add(new Song("C:/test/" + name,math.floor(getDurationWithMp3(file))))
      
    }
    cursor=hasard(0,listName.size)
  }

  
  
 // genere un nombre aleatoire 
  
  def hasard(inf:Int,sup:Int)={
   val  rand = new Random();
    rand.nextInt(sup - inf + 1) + inf;
  }
  //liste des fichier ayant les extentions .wav du repertoire passé en parametre
  private def listFilesNameForFolder(folder: File): Array[String] = {
    val directory = folder
    val myFiles = directory.list(new FilenameFilter() {

    //  def accept(directory: File, fileName: String): Boolean = return fileName.endsWith(".wav")
        def accept(directory: File, fileName: String): Boolean = return fileName.endsWith(".mp3")
    })
    size = myFiles.length
    myFiles
  }
}
//class represant une chanson
class Song(Nom: String,  duree: Double) {

  
 def getNom()=Nom

def getDuree()=duree

  override def toString(): String = this.Nom + " " + duree
}

//class servant a gerer le capteur de force 
class Poids_Presence extends Runnable {

  var threadPlayList: Playlist = _
  val ifk = new InterfaceKitPhidget()
  def init() {
   
    threadPlayList = new Playlist()
    ifk.addAttachListener(new AttachListener() {

      override def attached(arg0: AttachEvent) {
        try {
          ifk.setOutputState(7, true)
        } catch {
          case e: PhidgetException => e.printStackTrace()
        }
        println("attaché")
      }
    })
    ifk.addDetachListener(new DetachListener() {

      override def detached(arg0: DetachEvent) {
        println("Dattaché")
  threadPlayList.stop()
      }
    })
    ifk.addErrorListener(new ErrorListener() {

      def error(ee: ErrorEvent) {
      }
    })
    ifk.addInputChangeListener(new InputChangeListener() {

      def inputChanged(oe: InputChangeEvent) {
      }
    })
    ifk.addOutputChangeListener(new OutputChangeListener() {

      def outputChanged(oe: OutputChangeEvent) {
      }
    })
    ifk.addSensorChangeListener(new SensorChangeListener() {

      def sensorChanged(oe: SensorChangeEvent) {
      }
    })
  }

  def run() {
    threadPlayList.start()
    println("Lancement du thread")
    var temp = 0
    while (true) {
      val `val` = 1
      try {
        Thread.sleep(100)
        if (temp == 0) {
          println("Première entrée dans la boucle")
          ifk.open(317446)
          temp = 1
          ifk.waitForAttachment(1000)
        }
        val val_force = ifk.getSensorValue(6)
        if (val_force < 1) {
          println("stop music..")
         
          threadPlayList.stop()
        }
        println("valeur du 6 : " + `val`)
        val val_Presence = ifk.getSensorValue(7)
        if ((val_force < 5)) {
          ifk.setOutputState(7, false)
        } else {
          ifk.setOutputState(7, true)
        }
        println("valeur du 7 : " + ifk.getSensorValue(7))
      } catch {
        case e: PhidgetException => println("waiting for RFID attachment...")
        case e: InterruptedException => {
          e.printStackTrace()
          //break
        }
      }
      try {
        Thread.sleep(100)
      } catch {
        case e: InterruptedException => {
          e.printStackTrace()
          //break
        }
      }
    }
  }
}
}
