package ch.epfl.lara.engine.game.characters

import ch.epfl.lara.engine.game.environment.Room

import scala.collection.mutable

/**
  * A registry in which all the characters of the game are registered
  * @author Louis Vialar
  */
class CharacterRegistry {
  /**
    * Returns the first entity that is an instance of [[PlayerState]]
    */
  def player: CharacterState = characters.filter(_.isInstanceOf[PlayerState]).head

  val characters: mutable.ArrayBuffer[CharacterState] = mutable.ArrayBuffer()

  /**
    * Adds a new character to the register
    * @param character the character to add
    */
  def addCharacter(character: CharacterState): Unit = characters += character

  /**
    * Gets all the characters in a given room
    * @param room the room
    * @return all the characters that are in the room
    */
  def getCharacters(room: Room): List[CharacterState] = characters.filter(_.currentRoom == room).toList

  /**
    * Removes a character from the register
    * @param character the character to remove
    */
  def removeCharacter(character: CharacterState): Unit = characters -= character
}
