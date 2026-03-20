package avl

import cats.effect.IO
import cats.implicits._
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryCodec
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryCodec.{JsonBinaryDecodeOps, JsonBinaryEncodeOps}
import io.circe.generic.auto._
import io.circe.{Decoder, Encoder}
import weaver._
import weaver.scalacheck.Checkers
import org.scalacheck.Gen

/**
 * TDD Test Suite for AVL+ Tree Implementation
 * 
 * These tests are written BEFORE implementation to drive the design.
 * All tests should FAIL initially, then PASS as features are implemented.
 */
object AvlPlusTreeSpec extends SimpleIOSuite with Checkers {

  // Test data types
  case class TestKey(value: String) extends AnyVal
  case class TestValue(data: String, number: Int)
  
  implicit val testKeyOrdering: Ordering[TestKey] = Ordering.by(_.value)
  
  // Generators for property-based testing
  val genTestKey: Gen[TestKey] = Gen.alphaStr.map(TestKey)
  val genTestValue: Gen[TestValue] = for {
    data <- Gen.alphaStr
    num <- Gen.posNum[Int]
  } yield TestValue(data, num)
  
  val genKeyValuePair: Gen[(TestKey, TestValue)] = for {
    key <- genTestKey
    value <- genTestValue
  } yield (key, value)

  // Core AVL+ Tree Operations Tests
  
  test("empty tree should be created") {
    // This test will FAIL until AvlPlusTree companion object is implemented
    val emptyTree = AvlPlusTree.empty[TestKey, TestValue]
    IO(expect(emptyTree.isEmpty))
  }
  
  test("single insert should create tree with one element") {
    // This test will FAIL until insert method is implemented
    val key = TestKey("test")
    val value = TestValue("data", 42)
    
    for {
      emptyTree <- IO(AvlPlusTree.empty[TestKey, TestValue])
      treeWithOne <- emptyTree.insert(key, value)
    } yield expect(treeWithOne.size == 1) && expect(treeWithOne.contains(key))
  }
  
  test("lookup should return inserted value") {
    // This test will FAIL until lookup method is implemented
    val key = TestKey("lookup_test")
    val value = TestValue("found", 123)
    
    for {
      tree <- AvlPlusTree.empty[TestKey, TestValue].insert(key, value)
      result <- tree.lookup(key)
    } yield expect(result == Some(value))
  }
  
  test("lookup should return None for non-existent key") {
    // This test will FAIL until lookup method properly handles missing keys
    val key = TestKey("missing")
    
    for {
      emptyTree <- IO(AvlPlusTree.empty[TestKey, TestValue])
      result <- emptyTree.lookup(key)
    } yield expect(result.isEmpty)
  }
  
  test("multiple inserts should maintain sorted order") {
    // This test will FAIL until AVL tree maintains balance and order
    val keys = List("c", "a", "e", "b", "d").map(TestKey)
    val values = keys.zipWithIndex.map { case (k, i) => TestValue(k.value, i) }
    
    for {
      tree <- keys.zip(values).foldM(AvlPlusTree.empty[TestKey, TestValue]) { 
        case (acc, (k, v)) => acc.insert(k, v) 
      }
      inOrder <- tree.inOrderTraversal
    } yield expect(inOrder.map(_._1) == keys.sorted)
  }

  // AVL Balance Property Tests
  
  test("tree should maintain AVL invariant after insertions") {
    // This test will FAIL until AVL rotation logic is implemented
    forall(Gen.listOfN(20, genKeyValuePair)) { pairs =>
      pairs.foldM(AvlPlusTree.empty[TestKey, TestValue]) { case (acc, (k, v)) =>
        acc.insert(k, v)
      }.map { tree =>
        expect(tree.isBalanced) // Should verify height difference <= 1 for all nodes
      }
    }
  }
  
  test("tree should maintain BST property after rotations") {
    // This test will FAIL until proper BST ordering is maintained during rotations
    forall(Gen.listOfN(15, genKeyValuePair)) { pairs =>
      pairs.foldM(AvlPlusTree.empty[TestKey, TestValue]) { case (acc, (k, v)) =>
        acc.insert(k, v)
      }.map { tree =>
        expect(tree.isBinarySearchTree) // Should verify in-order traversal is sorted
      }
    }
  }

  // Authentication/Proof Tests (AVL+ specific features)
  
  test("should generate inclusion proof for existing key") {
    // This test will FAIL until proof generation is implemented
    val key = TestKey("prove_me")
    val value = TestValue("authenticated", 456)
    
    for {
      tree <- AvlPlusTree.empty[TestKey, TestValue].insert(key, value)
      proof <- tree.generateInclusionProof(key)
    } yield expect(proof.nonEmpty) // Proof should contain authentication path
  }
  
  test("inclusion proof should verify correctly") {
    // This test will FAIL until proof verification is implemented
    val key = TestKey("verify_test")
    val value = TestValue("verified", 789)
    
    for {
      tree <- AvlPlusTree.empty[TestKey, TestValue].insert(key, value)
      proof <- tree.generateInclusionProof(key)
      rootHash <- tree.rootHash
      isValid <- AvlPlusTree.verifyInclusionProof(key, value, proof, rootHash)
    } yield expect(isValid)
  }
  
  test("should generate exclusion proof for non-existent key") {
    // This test will FAIL until exclusion proof generation is implemented
    val existingKey = TestKey("exists")
    val missingKey = TestKey("missing")
    val value = TestValue("present", 111)
    
    for {
      tree <- AvlPlusTree.empty[TestKey, TestValue].insert(existingKey, value)
      proof <- tree.generateExclusionProof(missingKey)
    } yield expect(proof.nonEmpty) // Should contain proof that key doesn't exist
  }
  
  test("exclusion proof should verify correctly") {
    // This test will FAIL until exclusion proof verification is implemented
    val existingKey = TestKey("has_value")
    val missingKey = TestKey("no_value")
    val value = TestValue("exists", 222)
    
    for {
      tree <- AvlPlusTree.empty[TestKey, TestValue].insert(existingKey, value)
      proof <- tree.generateExclusionProof(missingKey)
      rootHash <- tree.rootHash
      isValid <- AvlPlusTree.verifyExclusionProof(missingKey, proof, rootHash)
    } yield expect(isValid)
  }

  // JsonBinaryCodec Integration Tests
  
  test("tree node should serialize with JsonBinaryCodec") {
    // This test will FAIL until AvlNode has proper Encoder/Decoder instances
    val key = TestKey("serialize_test")
    val value = TestValue("coded", 333)
    
    for {
      tree <- AvlPlusTree.empty[TestKey, TestValue].insert(key, value)
      rootNode <- tree.rootNode
      serialized <- rootNode.toBinary
    } yield expect(serialized.nonEmpty)
  }
  
  test("tree node should deserialize correctly with JsonBinaryCodec") {
    // This test will FAIL until proper round-trip serialization works
    val key = TestKey("roundtrip")
    val value = TestValue("back_and_forth", 444)
    
    for {
      tree <- AvlPlusTree.empty[TestKey, TestValue].insert(key, value)
      rootNode <- tree.rootNode
      serialized <- rootNode.toBinary
      deserialized <- serialized.fromBinary[AvlNode[TestKey, TestValue]]
    } yield expect(deserialized == Right(rootNode))
  }
  
  test("inclusion proof should serialize with JsonBinaryCodec") {
    // This test will FAIL until InclusionProof has Encoder/Decoder instances
    val key = TestKey("proof_serial")
    val value = TestValue("prove_it", 555)
    
    for {
      tree <- AvlPlusTree.empty[TestKey, TestValue].insert(key, value)
      proof <- tree.generateInclusionProof(key)
      serialized <- proof.toBinary
      deserialized <- serialized.fromBinary[InclusionProof[TestKey, TestValue]]
    } yield expect(deserialized == Right(proof))
  }

  // Immutability Tests
  
  test("insert should return new tree, not modify existing") {
    // This test will FAIL until proper immutable semantics are implemented
    val key1 = TestKey("immutable1")
    val key2 = TestKey("immutable2")
    val value1 = TestValue("first", 1)
    val value2 = TestValue("second", 2)
    
    for {
      tree1 <- AvlPlusTree.empty[TestKey, TestValue].insert(key1, value1)
      originalSize = tree1.size
      tree2 <- tree1.insert(key2, value2)
    } yield expect(tree1.size == originalSize) && expect(tree2.size == originalSize + 1)
  }
  
  test("operations should be referentially transparent") {
    // This test will FAIL until operations are pure and referentially transparent
    val key = TestKey("referential")
    val value = TestValue("transparent", 666)
    
    for {
      tree1 <- AvlPlusTree.empty[TestKey, TestValue].insert(key, value)
      tree2 <- AvlPlusTree.empty[TestKey, TestValue].insert(key, value)
      hash1 <- tree1.rootHash
      hash2 <- tree2.rootHash
    } yield expect(hash1 == hash2) // Same operations should produce same hash
  }

  // Error Handling Tests
  
  test("should handle insert with duplicate key") {
    // This test will FAIL until duplicate key handling is implemented
    val key = TestKey("duplicate")
    val value1 = TestValue("first", 777)
    val value2 = TestValue("second", 888)
    
    for {
      tree1 <- AvlPlusTree.empty[TestKey, TestValue].insert(key, value1)
      tree2 <- tree1.insert(key, value2)
      lookupResult <- tree2.lookup(key)
    } yield expect(lookupResult == Some(value2)) && expect(tree2.size == 1)
  }
  
  test("should fail gracefully with invalid proof") {
    // This test will FAIL until proof validation properly rejects invalid proofs
    val key = TestKey("invalid_proof")
    val value = TestValue("tampered", 999)
    val wrongValue = TestValue("wrong", 000)
    
    for {
      tree <- AvlPlusTree.empty[TestKey, TestValue].insert(key, value)
      proof <- tree.generateInclusionProof(key)
      rootHash <- tree.rootHash
      // Try to verify proof with wrong value (should fail)
      isValid <- AvlPlusTree.verifyInclusionProof(key, wrongValue, proof, rootHash)
    } yield expect(!isValid)
  }

  // Performance/Complexity Tests
  
  test("lookup should be O(log n) complexity") {
    // This test will FAIL until efficient tree structure provides logarithmic lookup
    val size = 1000
    val pairs = (1 to size).map(i => (TestKey(s"key_$i"), TestValue(s"value_$i", i)))
    
    for {
      tree <- pairs.foldM(AvlPlusTree.empty[TestKey, TestValue]) { case (acc, (k, v)) => acc.insert(k, v) }
      // In a balanced tree, height should be ~log2(n)
      height <- tree.height
      expectedMaxHeight = math.ceil(math.log(size) / math.log(2)).toInt + 2 // AVL allows +2 tolerance
    } yield expect(height <= expectedMaxHeight)
  }
  
  test("batch operations should maintain efficiency") {
    // This test will FAIL until batch operations are optimized
    val batchSize = 100
    val pairs = (1 to batchSize).map(i => (TestKey(s"batch_$i"), TestValue(s"data_$i", i)))
    
    for {
      tree <- pairs.foldM(AvlPlusTree.empty[TestKey, TestValue]) { case (acc, (k, v)) => acc.insert(k, v) }
      // All keys should be findable
      allFound <- pairs.traverse { case (k, v) => 
        tree.lookup(k).map(_ == Some(v))
      }
    } yield expect(allFound.forall(identity))
  }

  // Cryptographic Hash Tests
  
  test("tree hash should change when content changes") {
    // This test will FAIL until proper cryptographic hashing is implemented
    val key = TestKey("hash_test")
    val value1 = TestValue("original", 1111)
    val value2 = TestValue("modified", 2222)
    
    for {
      tree1 <- AvlPlusTree.empty[TestKey, TestValue].insert(key, value1)
      tree2 <- AvlPlusTree.empty[TestKey, TestValue].insert(key, value2)
      hash1 <- tree1.rootHash
      hash2 <- tree2.rootHash
    } yield expect(hash1 != hash2)
  }
  
  test("tree hash should be deterministic") {
    // This test will FAIL until hashing is deterministic across runs
    val key = TestKey("deterministic")
    val value = TestValue("consistent", 3333)
    
    for {
      tree1 <- AvlPlusTree.empty[TestKey, TestValue].insert(key, value)
      tree2 <- AvlPlusTree.empty[TestKey, TestValue].insert(key, value)
      hash1 <- tree1.rootHash
      hash2 <- tree2.rootHash
    } yield expect(hash1 == hash2)
  }
}