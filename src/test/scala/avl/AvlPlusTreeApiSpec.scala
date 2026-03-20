package avl

import cats.effect.IO
import weaver._

/**
 * TDD Tests for AVL+ Tree Public API
 * 
 * Defines the expected public interface before implementation.
 * These tests specify the complete API contract.
 */
object AvlPlusTreeApiSpec extends SimpleIOSuite {

  // Test the expected public API methods

  test("AvlPlusTree companion object should provide factory methods") {
    // This test will FAIL until companion object is implemented
    IO {
      val emptyTree = AvlPlusTree.empty[String, Int]
      val fromPairs = AvlPlusTree.fromSeq(Seq(("key1", 1), ("key2", 2)))
      
      expect(emptyTree.isEmpty) &&
      expect(fromPairs.size == 2)
    }
  }

  test("AvlPlusTree should implement core tree operations") {
    // This test will FAIL until core methods are implemented
    val tree = AvlPlusTree.empty[String, Int]
    
    for {
      // Insert operations
      tree1 <- tree.insert("a", 1)
      tree2 <- tree1.insert("b", 2)
      tree3 <- tree2.insert("c", 3)
      
      // Lookup operations
      lookupA <- tree3.lookup("a")
      lookupMissing <- tree3.lookup("missing")
      
      // Contains operations
      containsB <- tree3.contains("b")
      containsMissing <- tree3.contains("missing")
      
      // Size and structure queries
      size = tree3.size
      height <- tree3.height
      isEmpty = tree3.isEmpty
      
    } yield 
      expect(lookupA == Some(1)) &&
      expect(lookupMissing.isEmpty) &&
      expect(containsB) &&
      expect(!containsMissing) &&
      expect(size == 3) &&
      expect(height > 0) &&
      expect(!isEmpty)
  }

  test("AvlPlusTree should support update operations") {
    // This test will FAIL until update operations are implemented
    val tree = AvlPlusTree.empty[String, Int]
    
    for {
      tree1 <- tree.insert("key", 1)
      tree2 <- tree1.update("key", _ + 10) // Should update existing value
      tree3 <- tree2.updateWith("key")(v => Some(v.getOrElse(0) * 2)) // Functional update
      tree4 <- tree3.updateWith("new_key")(v => Some(v.getOrElse(100))) // Insert via update
      
      value1 <- tree2.lookup("key")
      value2 <- tree3.lookup("key")
      value3 <- tree4.lookup("new_key")
      
    } yield
      expect(value1 == Some(11)) &&
      expect(value2 == Some(22)) &&
      expect(value3 == Some(100))
  }

  test("AvlPlusTree should support removal operations") {
    // This test will FAIL until removal operations are implemented
    val tree = AvlPlusTree.empty[String, Int]
    
    for {
      tree1 <- tree.insert("a", 1).flatMap(_.insert("b", 2)).flatMap(_.insert("c", 3))
      tree2 <- tree1.remove("b")
      tree3 <- tree2.remove("missing") // Should be no-op
      
      sizeAfterRemoval = tree2.size
      containsB <- tree2.contains("b")
      stillHasA <- tree2.contains("a")
      stillHasC <- tree2.contains("c")
      
    } yield
      expect(sizeAfterRemoval == 2) &&
      expect(!containsB) &&
      expect(stillHasA) &&
      expect(stillHasC) &&
      expect(tree2.size == tree3.size) // No-op removal
  }

  test("AvlPlusTree should provide traversal operations") {
    // This test will FAIL until traversal methods are implemented
    val keys = List("c", "a", "e", "b", "d")
    val pairs = keys.zipWithIndex
    
    for {
      tree <- pairs.foldM(AvlPlusTree.empty[String, Int]) { case (acc, (k, v)) => acc.insert(k, v) }
      
      inOrder <- tree.inOrderTraversal
      preOrder <- tree.preOrderTraversal
      postOrder <- tree.postOrderTraversal
      keysOnly <- tree.keys
      valuesOnly <- tree.values
      allPairs <- tree.toSeq
      
    } yield
      expect(inOrder.map(_._1) == keys.sorted) && // In-order should be sorted
      expect(preOrder.nonEmpty) && // Should have pre-order traversal
      expect(postOrder.nonEmpty) && // Should have post-order traversal
      expect(keysOnly.toSet == keys.toSet) && // All keys present
      expect(valuesOnly.length == keys.length) && // All values present
      expect(allPairs.length == keys.length) // All pairs present
  }

  test("AvlPlusTree should provide range query operations") {
    // This test will FAIL until range query methods are implemented
    val pairs = (1 to 10).map(i => (s"key_${i.toString.padTo(2, '0')}", i))
    
    for {
      tree <- pairs.foldM(AvlPlusTree.empty[String, Int]) { case (acc, (k, v)) => acc.insert(k, v) }
      
      rangeInclusive <- tree.range("key_03", "key_07")
      rangeFrom <- tree.rangeFrom("key_08")
      rangeTo <- tree.rangeTo("key_03")
      
    } yield
      expect(rangeInclusive.length == 5) && // key_03 through key_07
      expect(rangeFrom.length == 3) && // key_08, key_09, key_10
      expect(rangeTo.length == 3) // key_01, key_02, key_03
  }

  test("AvlPlusTree should provide authentication/proof operations") {
    // This test will FAIL until proof generation methods are implemented
    val tree = AvlPlusTree.empty[String, Int]
    
    for {
      tree1 <- tree.insert("authenticated", 42)
      tree2 <- tree1.insert("verified", 84)
      
      // Root hash and authentication
      rootHash <- tree2.rootHash
      rootNode <- tree2.rootNode
      
      // Inclusion proofs
      inclusionProof <- tree2.generateInclusionProof("authenticated")
      verifyInclusion <- AvlPlusTree.verifyInclusionProof("authenticated", 42, inclusionProof, rootHash)
      
      // Exclusion proofs  
      exclusionProof <- tree2.generateExclusionProof("missing")
      verifyExclusion <- AvlPlusTree.verifyExclusionProof("missing", exclusionProof, rootHash)
      
      // Batch proofs
      batchProof <- tree2.generateBatchProof(List("authenticated", "verified"))
      verifyBatch <- AvlPlusTree.verifyBatchProof(List(("authenticated", 42), ("verified", 84)), batchProof, rootHash)
      
    } yield
      expect(rootHash.nonEmpty) &&
      expect(rootNode.key == "authenticated" || rootNode.key == "verified") && // Should be one of the keys (tree structure dependent)
      expect(inclusionProof.key == "authenticated") &&
      expect(verifyInclusion) &&
      expect(exclusionProof.missingKey == "missing") &&
      expect(verifyExclusion) &&
      expect(batchProof.proofs.length == 2) &&
      expect(verifyBatch)
  }

  test("AvlPlusTree should provide tree properties and invariants") {
    // This test will FAIL until property check methods are implemented
    val pairs = (1 to 20).map(i => (s"item_$i", i))
    
    for {
      tree <- pairs.foldM(AvlPlusTree.empty[String, Int]) { case (acc, (k, v)) => acc.insert(k, v) }
      
      isBalanced <- tree.isBalanced
      isBinarySearchTree <- tree.isBinarySearchTree
      heightProperty <- tree.checkHeightInvariant
      balanceFactors <- tree.allBalanceFactors
      
    } yield
      expect(isBalanced) && // AVL balance property
      expect(isBinarySearchTree) && // BST ordering property  
      expect(heightProperty) && // Height correctly computed
      expect(balanceFactors.forall(bf => math.abs(bf) <= 1)) // AVL balance factors
  }

  test("AvlPlusTree should provide serialization operations") {
    // This test will FAIL until serialization methods are implemented
    val pairs = List(("serialize", 1), ("deserialize", 2), ("roundtrip", 3))
    
    for {
      tree <- pairs.foldM(AvlPlusTree.empty[String, Int]) { case (acc, (k, v)) => acc.insert(k, v) }
      
      // Tree serialization
      serializedTree <- tree.serialize
      deserializedTree <- AvlPlusTree.deserialize[String, Int](serializedTree)
      
      // Individual node serialization (tested elsewhere, but part of API)
      rootNode <- tree.rootNode
      serializedNode <- rootNode.toBinary
      
    } yield
      expect(deserializedTree.isRight) &&
      expect(deserializedTree.exists(_.size == 3)) &&
      expect(serializedNode.nonEmpty)
  }

  test("AvlPlusTree should support functional operations") {
    // This test will FAIL until functional operation methods are implemented
    val numbers = (1 to 5).map(i => (s"num_$i", i))
    
    for {
      tree <- numbers.foldM(AvlPlusTree.empty[String, Int]) { case (acc, (k, v)) => acc.insert(k, v) }
      
      // Map operation (transform values)
      mappedTree <- tree.map(_ * 2)
      
      // Filter operation
      filteredTree <- tree.filter { case (k, v) => v % 2 == 0 }
      
      // Fold operation
      sum <- tree.fold(0)(_ + _._2)
      
      // ForEach operation (side effects) - using Ref for thread-safe mutation
      sideEffectCountRef <- cats.effect.Ref.of[IO, Int](0)
      _ <- tree.forEach { case (k, v) => sideEffectCountRef.update(_ + 1) }
      sideEffectCount <- sideEffectCountRef.get
      
    } yield
      expect(mappedTree.size == 5) &&
      expect(filteredTree.size < 5) && // Should filter out some elements
      expect(sum == (1 + 2 + 3 + 4 + 5)) &&
      expect(sideEffectCount == 5)
  }

  test("AvlPlusTree should provide merge and difference operations") {
    // This test will FAIL until tree combination methods are implemented
    for {
      tree1 <- List(("a", 1), ("b", 2), ("c", 3)).foldM(AvlPlusTree.empty[String, Int]) { case (acc, (k, v)) => acc.insert(k, v) }
      tree2 <- List(("c", 30), ("d", 4), ("e", 5)).foldM(AvlPlusTree.empty[String, Int]) { case (acc, (k, v)) => acc.insert(k, v) }
      
      // Merge operations
      merged <- tree1.merge(tree2) // Should combine, with tree2 values taking precedence for conflicts
      mergedWith <- tree1.mergeWith(tree2)((v1, v2) => v1 + v2) // Custom conflict resolution
      
      // Difference operations
      diff1 <- tree1.difference(tree2) // Keys in tree1 but not tree2
      diff2 <- tree2.difference(tree1) // Keys in tree2 but not tree1
      
      // Intersection
      intersection <- tree1.intersection(tree2) // Keys in both trees
      
    } yield
      expect(merged.size == 5) && // a, b, c, d, e
      expect(mergedWith.size == 5) &&
      expect(diff1.size == 2) && // a, b
      expect(diff2.size == 2) && // d, e
      expect(intersection.size == 1) // c
  }
}