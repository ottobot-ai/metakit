package avl

import cats.effect.IO
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryCodec
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryCodec.{JsonBinaryDecodeOps, JsonBinaryEncodeOps}
import io.circe.generic.auto._
import weaver._

/**
 * TDD Tests for AVL+ Tree Data Structures and Codecs
 * 
 * These tests define the expected data structures before implementation.
 * All tests should FAIL initially.
 */
object AvlDataStructuresSpec extends SimpleIOSuite {

  // Test the core data structures that will be needed

  test("AvlNode should have required structure") {
    // This test will FAIL until AvlNode case class is defined
    val key = TestKey("node_test")
    val value = TestValue("node_value", 42)
    val height = 1
    
    IO {
      val node = AvlNode.leaf(key, value)
      expect(node.key == key) &&
      expect(node.value == value) &&
      expect(node.height == height) &&
      expect(node.left.isEmpty) &&
      expect(node.right.isEmpty)
    }
  }

  test("AvlNode should calculate hash consistently") {
    // This test will FAIL until hash calculation is implemented
    val key = TestKey("hash_node")
    val value = TestValue("hashable", 123)
    
    IO {
      val node1 = AvlNode.leaf(key, value)
      val node2 = AvlNode.leaf(key, value)
      expect(node1.hash == node2.hash)
    }
  }

  test("AvlNode should have proper JsonBinaryCodec instances") {
    // This test will FAIL until Encoder/Decoder instances are implemented
    val key = TestKey("codec_test")
    val value = TestValue("encodable", 456)
    
    for {
      node <- IO(AvlNode.leaf(key, value))
      serialized <- node.toBinary
      deserialized <- serialized.fromBinary[AvlNode[TestKey, TestValue]]
    } yield expect(deserialized == Right(node))
  }

  test("InclusionProof should have required structure") {
    // This test will FAIL until InclusionProof case class is defined
    val key = TestKey("proof_key")
    val value = TestValue("proof_value", 789)
    val siblings = List(ProofNode(TestKey("sibling"), isLeft = true, hash = "sibling_hash"))
    
    IO {
      val proof = InclusionProof(key, value, siblings)
      expect(proof.key == key) &&
      expect(proof.value == value) &&
      expect(proof.siblings.length == 1) &&
      expect(proof.siblings.head.isLeft)
    }
  }

  test("InclusionProof should serialize with JsonBinaryCodec") {
    // This test will FAIL until proper serialization is implemented
    val key = TestKey("serializable_proof")
    val value = TestValue("proof_data", 987)
    val siblings = List(ProofNode(TestKey("proof_sibling"), isLeft = false, hash = "proof_hash"))
    
    for {
      proof <- IO(InclusionProof(key, value, siblings))
      serialized <- proof.toBinary
      deserialized <- serialized.fromBinary[InclusionProof[TestKey, TestValue]]
    } yield expect(deserialized == Right(proof))
  }

  test("ExclusionProof should have required structure") {
    // This test will FAIL until ExclusionProof case class is defined
    val missingKey = TestKey("not_found")
    val boundaryLeft = TestKey("left_bound")
    val boundaryRight = TestKey("right_bound")
    val siblings = List(ProofNode(TestKey("exclusion_sibling"), isLeft = true, hash = "exclusion_hash"))
    
    IO {
      val proof = ExclusionProof(missingKey, boundaryLeft, boundaryRight, siblings)
      expect(proof.missingKey == missingKey) &&
      expect(proof.leftBoundary == boundaryLeft) &&
      expect(proof.rightBoundary == boundaryRight) &&
      expect(proof.siblings.nonEmpty)
    }
  }

  test("ExclusionProof should serialize with JsonBinaryCodec") {
    // This test will FAIL until proper serialization is implemented
    val missingKey = TestKey("absent")
    val leftBound = TestKey("before")
    val rightBound = TestKey("after")
    val siblings = List(ProofNode(TestKey("exclusion_node"), isLeft = false, hash = "exc_hash"))
    
    for {
      proof <- IO(ExclusionProof(missingKey, leftBound, rightBound, siblings))
      serialized <- proof.toBinary
      deserialized <- serialized.fromBinary[ExclusionProof[TestKey, TestValue]]
    } yield expect(deserialized == Right(proof))
  }

  test("ProofNode should have proper structure") {
    // This test will FAIL until ProofNode case class is defined
    val key = TestKey("proof_node_key")
    val hash = "node_hash_value"
    val isLeft = true
    
    IO {
      val proofNode = ProofNode(key, isLeft, hash)
      expect(proofNode.key == key) &&
      expect(proofNode.isLeft == isLeft) &&
      expect(proofNode.hash == hash)
    }
  }

  test("TreeHash should provide cryptographic hashing") {
    // This test will FAIL until TreeHash object is implemented
    val data = "test_hash_input"
    
    IO {
      val hash1 = TreeHash.hashString(data)
      val hash2 = TreeHash.hashString(data)
      val differentHash = TreeHash.hashString(data + "_different")
      
      expect(hash1 == hash2) && // Deterministic
      expect(hash1 != differentHash) && // Different inputs produce different hashes
      expect(hash1.nonEmpty) // Produces actual hash value
    }
  }

  test("TreeHash should hash key-value pairs consistently") {
    // This test will FAIL until key-value hashing is implemented
    val key = TestKey("hashable_key")
    val value = TestValue("hashable_value", 111)
    
    IO {
      val hash1 = TreeHash.hashKeyValue(key, value)
      val hash2 = TreeHash.hashKeyValue(key, value)
      expect(hash1 == hash2) && expect(hash1.nonEmpty)
    }
  }

  test("TreeHash should combine hashes for internal nodes") {
    // This test will FAIL until hash combination is implemented
    val leftHash = "left_child_hash"
    val rightHash = "right_child_hash"
    
    IO {
      val combined1 = TreeHash.combineHashes(leftHash, rightHash)
      val combined2 = TreeHash.combineHashes(leftHash, rightHash)
      val differentOrder = TreeHash.combineHashes(rightHash, leftHash)
      
      expect(combined1 == combined2) && // Deterministic
      expect(combined1 != differentOrder) && // Order matters
      expect(combined1.nonEmpty) // Produces hash
    }
  }

  // Helper data types used in tests (should match those in AvlPlusTreeSpec)
  case class TestKey(value: String) extends AnyVal
  case class TestValue(data: String, number: Int)

  implicit val testKeyOrdering: Ordering[TestKey] = Ordering.by(_.value)
}