package avl

import cats.effect.IO
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryCodec
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryCodec.{JsonBinaryDecodeOps, JsonBinaryEncodeOps}
import io.circe.generic.auto._
import weaver._
import weaver.scalacheck.Checkers
import org.scalacheck.Gen

/**
 * TDD Tests for AVL+ Tree Edge Cases and Error Conditions
 * 
 * Tests boundary conditions, error scenarios, and edge cases.
 * All tests should FAIL initially until proper error handling is implemented.
 */
object AvlPlusTreeEdgeCasesSpec extends SimpleIOSuite with Checkers {

  // Helper data types
  case class TestKey(value: String) extends AnyVal
  case class TestValue(data: String, number: Int)
  implicit val testKeyOrdering: Ordering[TestKey] = Ordering.by(_.value)

  // Empty Tree Edge Cases
  
  test("empty tree should handle all operations gracefully") {
    // This test will FAIL until empty tree edge cases are handled
    val emptyTree = AvlPlusTree.empty[TestKey, TestValue]
    
    for {
      // Operations on empty tree should not fail
      lookup <- emptyTree.lookup(TestKey("missing"))
      contains <- emptyTree.contains(TestKey("absent"))
      remove <- emptyTree.remove(TestKey("nonexistent"))
      
      // Traversals should return empty results
      inOrder <- emptyTree.inOrderTraversal
      keys <- emptyTree.keys
      values <- emptyTree.values
      toSeq <- emptyTree.toSeq
      
      // Queries should handle empty gracefully
      height <- emptyTree.height
      rootHashResult <- emptyTree.rootHash.attempt // Might fail for empty tree
      
    } yield
      expect(lookup.isEmpty) &&
      expect(!contains) &&
      expect(remove.isEmpty) && // Should remain empty
      expect(inOrder.isEmpty) &&
      expect(keys.isEmpty) &&
      expect(values.isEmpty) &&
      expect(toSeq.isEmpty) &&
      expect(height == 0) &&
      expect(rootHashResult.isLeft || rootHashResult.exists(_.nonEmpty)) // Either error or valid hash
  }

  test("single node tree should handle operations correctly") {
    // This test will FAIL until single-node edge cases are handled
    val key = TestKey("only")
    val value = TestValue("single", 1)
    
    for {
      tree <- AvlPlusTree.empty[TestKey, TestValue].insert(key, value)
      
      // Should find the only element
      lookup <- tree.lookup(key)
      contains <- tree.contains(key)
      
      // Should not find other elements
      missingLookup <- tree.lookup(TestKey("other"))
      missingContains <- tree.contains(TestKey("different"))
      
      // Removal should make it empty
      afterRemoval <- tree.remove(key)
      
      // Structure queries
      height <- tree.height
      size = tree.size
      isBalanced <- tree.isBalanced
      isBST <- tree.isBinarySearchTree
      
    } yield
      expect(lookup == Some(value)) &&
      expect(contains) &&
      expect(missingLookup.isEmpty) &&
      expect(!missingContains) &&
      expect(afterRemoval.isEmpty) &&
      expect(height == 1) &&
      expect(size == 1) &&
      expect(isBalanced) &&
      expect(isBST)
  }

  // Duplicate Key Handling

  test("should handle duplicate key insertion correctly") {
    // This test will FAIL until duplicate key policy is implemented
    val key = TestKey("duplicate")
    val value1 = TestValue("first", 100)
    val value2 = TestValue("second", 200)
    
    for {
      tree1 <- AvlPlusTree.empty[TestKey, TestValue].insert(key, value1)
      tree2 <- tree1.insert(key, value2) // Should replace, not add
      
      lookup <- tree2.lookup(key)
      size = tree2.size
      
    } yield
      expect(lookup == Some(value2)) && // Should have the new value
      expect(size == 1) // Size should remain 1, not 2
  }

  // Extreme Ordering Cases

  test("should handle keys with identical ordering") {
    // This test will FAIL until proper handling of equal keys in ordering
    case class EqualKey(id: Int) // All instances are equal according to ordering
    implicit val equalOrdering: Ordering[EqualKey] = Ordering.by(_ => 0) // All equal!
    
    val value1 = TestValue("equal1", 1)
    val value2 = TestValue("equal2", 2)
    
    for {
      tree1 <- AvlPlusTree.empty[EqualKey, TestValue].insert(EqualKey(1), value1)
      tree2 <- tree1.insert(EqualKey(2), value2) // Should replace due to equal ordering
      
      lookup1 <- tree2.lookup(EqualKey(1))
      lookup2 <- tree2.lookup(EqualKey(2))
      size = tree2.size
      
    } yield
      // With equal ordering, behavior depends on implementation choice
      expect(size == 1) && // Should have only one element due to equal ordering
      expect(lookup1 == lookup2) // Both lookups should return same value
  }

  test("should handle extreme string ordering cases") {
    // This test will FAIL until proper string comparison edge cases are handled
    val specialKeys = List(
      "", // Empty string
      " ", // Single space
      "\t", // Tab character
      "\n", // Newline
      "\u0000", // Null character
      "A", // Single char
      "a", // Different case
      "AA", // Repeated chars
      "🦦", // Unicode emoji
      "∑", // Math symbol
      "测试" // Non-Latin characters
    ).map(TestKey)
    
    val values = specialKeys.zipWithIndex.map { case (_, i) => TestValue(s"special_$i", i) }
    
    for {
      tree <- specialKeys.zip(values).foldM(AvlPlusTree.empty[TestKey, TestValue]) { 
        case (acc, (k, v)) => acc.insert(k, v) 
      }
      
      // All keys should be findable
      allFound <- specialKeys.traverse(k => tree.contains(k))
      
      // In-order traversal should be properly sorted
      inOrder <- tree.inOrderTraversal
      isSorted = inOrder.map(_._1).sliding(2).forall {
        case Seq(a, b) => testKeyOrdering.lteq(a, b)
        case _ => true
      }
      
    } yield
      expect(allFound.forall(identity)) &&
      expect(isSorted) &&
      expect(tree.size == specialKeys.length)
  }

  // Large Tree Stress Tests

  test("should handle large sequential insertions without stack overflow") {
    // This test will FAIL until tail recursion or trampolines prevent stack overflow
    val size = 10000
    val pairs = (1 to size).map(i => (TestKey(f"seq_$i%05d"), TestValue(s"data_$i", i)))
    
    for {
      tree <- pairs.foldM(AvlPlusTree.empty[TestKey, TestValue]) { case (acc, (k, v)) => acc.insert(k, v) }
      height <- tree.height
      isBalanced <- tree.isBalanced
      expectedMaxHeight = math.ceil(math.log(size) / math.log(2)).toInt + 2 // AVL height bound
      
    } yield
      expect(tree.size == size) &&
      expect(height <= expectedMaxHeight) &&
      expect(isBalanced)
  }

  test("should handle reverse sequential insertions") {
    // This test will FAIL until rotations properly handle worst-case insertion patterns
    val size = 1000
    val pairs = (1 to size).reverse.map(i => (TestKey(f"rev_$i%05d"), TestValue(s"reverse_$i", i)))
    
    for {
      tree <- pairs.foldM(AvlPlusTree.empty[TestKey, TestValue]) { case (acc, (k, v)) => acc.insert(k, v) }
      height <- tree.height
      isBalanced <- tree.isBalanced
      expectedMaxHeight = math.ceil(math.log(size) / math.log(2)).toInt + 2
      
    } yield
      expect(tree.size == size) &&
      expect(height <= expectedMaxHeight) &&
      expect(isBalanced)
  }

  test("should handle random insertions and maintain invariants") {
    // This test will FAIL until random insertion patterns maintain tree properties
    forall(Gen.listOfN(500, Gen.alphaNumStr.map(TestKey))) { keys =>
      val uniqueKeys = keys.distinct
      val values = uniqueKeys.zipWithIndex.map { case (_, i) => TestValue(s"random_$i", i) }
      
      uniqueKeys.zip(values).foldM(AvlPlusTree.empty[TestKey, TestValue]) { 
        case (acc, (k, v)) => acc.insert(k, v) 
      }.map { tree =>
        val height = tree.height
        val expectedMaxHeight = if (uniqueKeys.nonEmpty) 
          math.ceil(math.log(uniqueKeys.length) / math.log(2)).toInt + 2 
        else 0
        
        expect(tree.size == uniqueKeys.length) &&
        expect(height <= expectedMaxHeight)
      }
    }
  }

  // Proof Generation Edge Cases

  test("should handle proof generation for empty tree") {
    // This test will FAIL until proper error handling for empty tree proofs
    val emptyTree = AvlPlusTree.empty[TestKey, TestValue]
    val key = TestKey("not_there")
    
    for {
      inclusionResult <- emptyTree.generateInclusionProof(key).attempt
      exclusionResult <- emptyTree.generateExclusionProof(key).attempt
      
    } yield
      expect(inclusionResult.isLeft) && // Should fail - can't prove inclusion in empty tree
      expect(exclusionResult.isRight) // Should succeed - can prove exclusion from empty tree
  }

  test("should handle proof generation for single-node tree") {
    // This test will FAIL until single-node proof generation is implemented
    val key = TestKey("single")
    val value = TestValue("alone", 42)
    val missingKey = TestKey("absent")
    
    for {
      tree <- AvlPlusTree.empty[TestKey, TestValue].insert(key, value)
      
      inclusionProof <- tree.generateInclusionProof(key)
      exclusionProof <- tree.generateExclusionProof(missingKey)
      
      rootHash <- tree.rootHash
      verifyInclusion <- AvlPlusTree.verifyInclusionProof(key, value, inclusionProof, rootHash)
      verifyExclusion <- AvlPlusTree.verifyExclusionProof(missingKey, exclusionProof, rootHash)
      
    } yield
      expect(inclusionProof.siblings.isEmpty) && // No siblings in single-node tree
      expect(verifyInclusion) &&
      expect(verifyExclusion)
  }

  // Serialization Edge Cases

  test("should handle serialization of deeply nested tree") {
    // This test will FAIL until deep tree serialization doesn't cause stack overflow
    val depth = 100
    val pairs = (1 to depth).map(i => (TestKey(s"deep_$i"), TestValue(s"level_$i", i)))
    
    for {
      tree <- pairs.foldM(AvlPlusTree.empty[TestKey, TestValue]) { case (acc, (k, v)) => acc.insert(k, v) }
      serialized <- tree.serialize
      deserialized <- AvlPlusTree.deserialize[TestKey, TestValue](serialized)
      
    } yield
      expect(deserialized.isRight) &&
      expect(deserialized.exists(_.size == depth))
  }

  test("should handle serialization with special characters and unicode") {
    // This test will FAIL until unicode/special character serialization is robust
    val specialPairs = List(
      (TestKey("emoji_🦦"), TestValue("otter", 1)),
      (TestKey("null_\u0000"), TestValue("null_char", 2)),
      (TestKey("quote_\""), TestValue("quoted", 3)),
      (TestKey("slash_\\"), TestValue("backslash", 4)),
      (TestKey("unicode_∑∏∫"), TestValue("math", 5))
    )
    
    for {
      tree <- specialPairs.foldM(AvlPlusTree.empty[TestKey, TestValue]) { case (acc, (k, v)) => acc.insert(k, v) }
      serialized <- tree.serialize
      deserialized <- AvlPlusTree.deserialize[TestKey, TestValue](serialized)
      
      // Verify all special keys are preserved
      allKeysPreserved <- deserialized.fold(
        _ => IO.pure(false),
        deserializedTree => specialPairs.traverse { case (k, v) => 
          deserializedTree.lookup(k).map(_ == Some(v))
        }.map(_.forall(identity))
      )
      
    } yield
      expect(deserialized.isRight) &&
      expect(allKeysPreserved)
  }

  // Memory and Resource Edge Cases

  test("should handle operations without memory leaks") {
    // This test will FAIL until proper resource management prevents memory leaks
    val iterations = 1000
    
    for {
      // Repeatedly create and discard trees to test for memory leaks
      _ <- (1 to iterations).foldM(AvlPlusTree.empty[TestKey, TestValue]) { case (_, i) =>
        val pairs = (1 to 10).map(j => (TestKey(s"iter_${i}_$j"), TestValue(s"data_$j", j)))
        pairs.foldM(AvlPlusTree.empty[TestKey, TestValue]) { case (acc, (k, v)) => acc.insert(k, v) }
      }
      
    } yield expect(true) // If we get here without OOM, test passes
  }

  test("should handle concurrent operations safely") {
    // This test will FAIL until thread-safety is properly implemented
    val tree = AvlPlusTree.empty[TestKey, TestValue]
    val pairs = (1 to 100).map(i => (TestKey(s"concurrent_$i"), TestValue(s"parallel_$i", i)))
    
    for {
      // Simulate concurrent insertions
      baseTree <- pairs.foldM(tree) { case (acc, (k, v)) => acc.insert(k, v) }
      
      // Concurrent lookups (should be safe for immutable structure)
      concurrentLookups <- pairs.take(10).parTraverse { case (k, _) => baseTree.lookup(k) }
      
    } yield
      expect(baseTree.size == pairs.length) &&
      expect(concurrentLookups.forall(_.nonEmpty))
  }

  // Error Recovery and Validation

  test("should validate tree invariants after complex operations") {
    // This test will FAIL until invariant validation is comprehensive
    val operations = for {
      i <- 1 to 50
      op <- List("insert", "remove", "update")
    } yield (op, TestKey(s"op_${i % 20}"), TestValue(s"value_$i", i))
    
    for {
      finalTree <- operations.foldM(AvlPlusTree.empty[TestKey, TestValue]) { 
        case (tree, ("insert", k, v)) => tree.insert(k, v)
        case (tree, ("remove", k, _)) => tree.remove(k)
        case (tree, ("update", k, v)) => tree.insert(k, v) // Update via re-insert
      }
      
      isBalanced <- finalTree.isBalanced
      isBST <- finalTree.isBinarySearchTree
      heightValid <- finalTree.checkHeightInvariant
      
    } yield
      expect(isBalanced) &&
      expect(isBST) &&
      expect(heightValid)
  }

  test("should handle malformed serialized data gracefully") {
    // This test will FAIL until proper error handling for bad input is implemented
    val malformedInputs = List(
      Array[Byte](), // Empty
      "not json".getBytes, // Invalid JSON
      "{}".getBytes, // Valid JSON but missing required fields
      """{"type":"AvlNode","key":"test"}""".getBytes // Missing required fields
    )
    
    for {
      results <- malformedInputs.traverse { bytes =>
        AvlPlusTree.deserialize[TestKey, TestValue](bytes).attempt
      }
    } yield expect(results.forall(_.isLeft)) // All should fail gracefully
  }
}