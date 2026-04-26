---
title: cardano redemption codes are private keys
tags: cardano, EdDSA
---

# Cardano redemption codes are private keys

The McDermott, Will & Schulte report on the Cardano unredeemed ADA is out,
and can be found [here](https://www.adaredemptiontransparency.com/investigative-report.pdf).

As the developer who
[wrote the code](https://github.com/IntersectMBO/cardano-ledger/pull/1933)
that deleted the "redeem" address UTxO (unspent transaction outputs)
during the Allegra hard fork (and increased the Cardano reserves by the corresponding amount),
I want to point out some errors that I found in the report.

In [June 2021](https://github.com/IntersectMBO/cardano-ledger/commit/72f924f7badbda4d6ff40bcf3056b2ff999396f6)
I added a brief note about this to the Shelley ledger specification errata.
As far as I'm aware, that was the only public written description of this at the time.
There was no accompanying announcement, blog post, or release-notes entry that I am aware of.

We all stand to gain from a clear technical record of the past.

For reference, the version of the report that I read has this SHA-256:

```shell
$ sha256sum investigative-report.pdf
4769bc698432af1bc64282cfca560adcd090d9e98784b93e5b35f81cc64cfd43
```

The main point I wish to make is:

The redemption codes corresponding to the deleted UTxO during the Allegra hard fork are
[Ed25519](https://en.wikipedia.org/wiki/EdDSA#Ed25519) private keys.

Additionally, I would like to clear up confusion around the terms:

* private key
* wallet
* seed used for an HD wallet

But first, I need to be clear about what I mean by "redemption codes".

## Clarification of the term "redemption code"

There are many aspects of the Cardano voucher program that are unknown to me,
and are not within the scope of this post.

In particular, I do not know:

* the details of the RSCoin blockchain described in section IV.A of the report (pp. 36-38),
* the details of how redemption codes were delivered to voucher holders,
  including the distinctions the report draws between "Regular Vend", "Force Vend",
  and "Paper Vend" delivery (pp. 37-38),
* the details of the Japanese-language user policies and their English translations
  (section IV.B.2).

What I *do* know, and what this post is about, is what the Daedalus wallet did
with a redemption code once a voucher holder had one.
Regardless of how a given redemption code reached its holder,
the Daedalus wallet ultimately interpreted every redemption code as an Ed25519 private key.[^papervend]

[^papervend]: The wallet backend `cardano-sl` actually had two separate decoding paths for redemption codes.
The one shown later in this post, `fromRedemptionCode`, handled non-paper vends:
base64 decode to 32 bytes, then `redeemDeterministicKeyGen`.
For paper-vended codes, a sibling function `fromRedemptionCodePaper` in the same file does a
base58 decoding and an AES decryption step, with the AES key derived from a mnemonic printed
alongside the encoded code on the paper certificate.
Both paths feed into the same `redeemDeterministicKeyGen` and produce a `RedeemSecretKey`, i.e. an Ed25519 private key.
The mnemonic on a paper certificate served as a passphrase for decrypting the code printed beside it,
not as an HD-wallet seed used to derive keys.

When I refer to a "redemption code" in the rest of this post, I am referring
**only** to the codes that the Daedalus wallet used to spend redeem address UTxO.

## What the report says about the redemption codes

Section IV.B (p. 39) of the report states:

> Voucher Holders Received Redemption Codes Not Private Keys

In the same section, on page 40, it also states:

> Voucher Holders needed the Redemption Code to convert a Voucher Certificate to ada –
but it was not itself a traditional private key.

And, still on page 40, it makes the stronger claim:[^password]

> The Redemption Code is more aptly described as a "password" because it is the code
needed to redeem ada and does not give the holder the technical ability to control
or transact ada held in any particular digital address.

[^password]: The report's two uses of the word "password" are not consistent with each other.
On page 39 the report defines a private key as
"essentially individual passwords used to denote ownership in a particular blockchain address",
i.e., it uses "password" as a synonym for "private key".
On page 40, in the quote above, the same word is instead being used as a *contrast* to "private key",
to mean a code that "does not give the holder the technical ability to control or transact ada".

I disagree with these statements, and will now provide source code which supports my position.

## What the source code demonstrates about the redemption codes

The Daedalus wallet was responsible for processing the redemption codes.
The redemption-code logic itself lived in the wallet backend, `cardano-sl`,
which the Daedalus Electron frontend connected to.

[Here](https://github.com/input-output-hk/cardano-sl/blob/1499214d93767b703b9599369a431e67d83f10a2/wallet/src/Cardano/Wallet/WalletLayer/Kernel/Conv.hs#L92-L104)
is the code which Daedalus used to process the redemption codes:

```haskell
-- | Decode redemption key for non-paper wallet
--
-- See also comments for 'fromRedemptionCodePaper'.
fromRedemptionCode :: Monad m
                   => V1.ShieldedRedemptionCode
                   -> ExceptT InvalidRedemptionCode m RedeemSecretKey
fromRedemptionCode (V1.ShieldedRedemptionCode crSeed) = do
    bs <- withExceptT (const $ InvalidRedemptionCodeInvalidBase64 crSeed) $
            asum [ exceptT $ B64.decode    crSeed
                 , exceptT $ B64.decodeUrl crSeed
                 ]
    exceptT $ maybe (Left $ InvalidRedemptionCodeNot32Bytes crSeed) (Right . snd) $
      redeemDeterministicKeyGen bs
```

While the code is admittedly dense,
the type signature alone shows that `fromRedemptionCode` converts
a `V1.ShieldedRedemptionCode` into a `RedeemSecretKey`.
Moreover, it is not too hard to see that all the code does is remove the Base64 encoding.

The variable name `crSeed` above refers to an **Ed25519 seed**,
the 32 bytes of secret material that constitute an Ed25519 private key in the sense of
[RFC 8032](https://datatracker.ietf.org/doc/html/rfc8032#section-5.1.5).
This is a different (though also standard) use of the word "seed" than the **HD wallet seed** discussed later in this post.
An Ed25519 seed *is* the private key, modulo a trivial encoding.
An HD wallet seed, by contrast, is a master secret from which many private keys are derived.

The next question you should ask is: what exactly is a `RedeemSecretKey`?

[Here](https://github.com/input-output-hk/cardano-sl/blob/1499214d93767b703b9599369a431e67d83f10a2/crypto/Pos/Crypto/Signing/Types/Redeem.hs#L71-L73)
is the answer:

```haskell
-- | Wrapper around 'Ed25519.SecretKey'.
newtype RedeemSecretKey = RedeemSecretKey Ed25519.SecretKey
    deriving (Eq, Ord, Show, Generic, NFData, Hashable)
```

In other words, a redemption code is nothing more than an **Ed25519 private key**.

To drive the point home,
[`redeemDeterministicKeyGen`](https://github.com/input-output-hk/cardano-sl/blob/1499214d93767b703b9599369a431e67d83f10a2/crypto/Pos/Crypto/Signing/Redeem.hs#L38-L45)
(the function `fromRedemptionCode` calls on the decoded bytes) does nothing more than feed those
32 bytes directly into the standard Ed25519 secret-key constructor:

```haskell
-- | Create key pair deterministically from 32 bytes.
redeemDeterministicKeyGen
    :: BS.ByteString
    -> Maybe (RedeemPublicKey, RedeemSecretKey)
redeemDeterministicKeyGen seed =
    case maybeCryptoError $ Ed25519.secretKey $ fromByteStringToBytes seed of
        Just r -> Just (RedeemPublicKey $ Ed25519.toPublic r, RedeemSecretKey r)
        Nothing -> fail "Pos.Crypto.Signing.Redeem.hs redeemDeterministicKeyGen failed"
```

The key expression is `Ed25519.secretKey $ fromByteStringToBytes seed`:
there is no hashing, no derivation, and no mixing with any other material.
The bytes from the redemption code are passed as-is to the Ed25519 library.

Note that the adjectives "private", "secret", and "signing" are all interchangeable in this context.

Once Daedalus deserializes this private key,
it can produce a digital signature granting
authentication and authorization to transfer the
ADA to any addresses it wants.

### And the Byron ledger code agrees

The code above shows what the Daedalus wallet did with the redemption codes.
We can compare it with the Byron ledger code, which dictates what the protocol itself does.

Byron addresses have an `AddrType`, and the variant used for redeem addresses is
[`ATRedeem`](https://github.com/IntersectMBO/cardano-ledger/blob/72f924f7badbda4d6ff40bcf3056b2ff999396f6/byron/ledger/impl/src/Cardano/Chain/Common/AddrSpendingData.hs#L77-L81),
encoded with the discriminator byte `2`.
The corresponding spending data is
[`RedeemASD !RedeemVerificationKey`](https://github.com/IntersectMBO/cardano-ledger/blob/72f924f7badbda4d6ff40bcf3056b2ff999396f6/byron/ledger/impl/src/Cardano/Chain/Common/AddrSpendingData.hs#L36-L46),
whose Haskell source comment reads:

> Funds can be spent by revealing a 'RedeemVerificationKey' and providing a valid signature

In other words, the Byron protocol specifies that a redeem UTxO is spent the same way every other
Byron UTxO is spent: by revealing a public key and a digital signature over the transaction.
Any code that turns a redemption code into something that can produce that signature is
necessarily producing an Ed25519 private key, because that is what the protocol requires.

We can also see this directly in the protocol's verification function.
The redeem-address verification key
[is a wrapper around `Ed25519.PublicKey`](https://github.com/IntersectMBO/cardano-ledger/blob/72f924f7badbda4d6ff40bcf3056b2ff999396f6/byron/crypto/src/Cardano/Crypto/Signing/Redeem/VerificationKey.hs#L54-L58),
and the [signature-verification function](https://github.com/IntersectMBO/cardano-ledger/blob/72f924f7badbda4d6ff40bcf3056b2ff999396f6/byron/crypto/src/Cardano/Crypto/Signing/Redeem/Signature.hs#L89-L96)
is `Ed25519.verify`:

```haskell
-- | Verify raw 'ByteString'
verifyRedeemSigRaw
  :: RedeemVerificationKey
  -> ByteString
  -> RedeemSignature Raw
  -> Bool
verifyRedeemSigRaw (RedeemVerificationKey k) x (RedeemSignature s) =
  Ed25519.verify k x s
```


## Confusion of terms

I will now clarify what some key (pun!) terms mean,
and why I believe that the report uses them incorrectly.

From the report (p. 39):

> Like public keys, private keys similarly consist of multi-digit alphanumeric strings.
However, unlike public keys—which are identifiable to the public and used to identify a digital
wallet—private keys are only known by the owner of the digital wallet and are used by the owner
to access, manage, and restore a digital wallet.
In sum, private keys give digital wallet owners the
following key features: (1) sole custody; (2) the ability to both receive and send transactions from
a digital wallet; and (3) the ability to regenerate the private key using a recovery or seed phrase.
The Redemption Code did not have all the key features typically associated with a cryptocurrency
private key.

I believe that the report is conflating three distinct concepts: **private key**,
**seed of an HD wallet**, and **mnemonic** (also called a "seed phrase" or "recovery phrase").


### A note on "traditional"

Several of the report's claims are about whether the redemption code is a
"traditional private key" (e.g., pp. 39, 40, 41).
There is no technical distinction between a "traditional" and a "non-traditional" private key,
and Ed25519 is as "traditional" as you get.


### Private key

The term "private key" comes from
[Public-key cryptography](https://en.wikipedia.org/wiki/Public-key_cryptography)
and is applicable to cryptography in general, not just to blockchains.
Cardano uses private keys, and in particular Ed25519 keys, to produce digital signatures.

Digital signatures provide a mechanism for authorization and authentication of UTxO.
In particular, given a private key `S` and a corresponding public key `V`,
any UTxO whose payment credential contains (the hash of) `V` can be spent by producing
a digital signature using `S` of the relevant transaction details.

Note that none of this requires any notion of "wallet" or the regeneration of other keys.

#### Side Note

Sentences like "deleting a private key from the blockchain" are nonsensical.
Blockchain protocols deal exclusively in public information,
so while signatures appear on a blockchain, private keys never do.

### Wallets

I have seen an enormous amount of confusion around the term "wallet" on blockchains.
It is important to understand that at the protocol level, Cardano
**has no notion of wallets**.

A "wallet" does not have as precise a definition as, say, an Ed25519 private key.
When people in the blockchain space refer to a "wallet", they usually mean some combination of:

* The software used to manage secrets and interact with a blockchain.
* The secrets themselves (like private keys) which are managed by such software.

While a private key is the kind of secret data that a wallet would manage,
it should not be confused with the wallet itself.
The confusion probably stems from the idea of a seed for an HD wallet.

### Seeds for HD wallets

The concept of a "hierarchical deterministic wallet" (or HD wallet for short) came from
[BIP 32](https://github.com/bitcoin/bips/blob/eabb63cb53ed487309249a59f76051d404fd4632/bip-0032.mediawiki#abstract).

The idea is to use a single secret, called the **seed**, to generate a family of keypairs.[^chaincode]
The seed itself is not a private key, it is a secret used to generate private keys.

[^chaincode]: A bit of Cardano lore tangential to this post:
the HD derivation scheme used in Byron attached a 32-byte "chain code" to every public key,
and Cardano's **bootstrap witnesses** (the witnesses to the main Byron `PubKey` addresses
that survived into the Shelley era) publish those chain codes on-chain in order to be verified.
BIP 32 cautions that extended public keys (public key + chain code)
"must be treated more carefully than regular public keys", which sits awkwardly with this.
None of this touches redemption codes, though, since they never used HD derivation
and never had a chain code attached in the first place.

When people think of wallets in the blockchain space, they often assume that it is doing some
kind of hierarchical derivation of keys based on an initial secret.

For a host of good reasons, it is often better to use a mnemonic, as specified in
[BIP 39](https://github.com/bitcoin/bips/blob/eabb63cb53ed487309249a59f76051d404fd4632/bip-0039.mediawiki#abstract),
than a raw seed.
But this is entirely about presentation (human vs machine),
a seed and a mnemonic can be converted between each other using publicly known algorithms.
The terms "seed phrase" and "recovery phrase" are synonymous with this mnemonic.

This is the context for another statement in the report (p. 40):

> The Redemption Code was an alphanumeric code that is completely different from a private key
or recovery phrase (i.e., a 12- or 24-word phrase).

The distinction being drawn here is about presentation, not substance.
The same 32 bytes of secret material can be written out as base64 alphanumeric text, as hexadecimal, as a BIP 39 phrase, etc.
The choice of encoding is irrelevant to what the underlying bytes are used for.
As shown in the previous section, the bytes encoded in a Cardano redemption code are, once decoded, an Ed25519 private key.

### Recap

In light of the above definitions, we can break down the quote given earlier
from the report (on page 39), describing private keys:

* **"used by the owner to access, manage, and restore a digital wallet."**
This describes a seed for an HD wallet, not a private key.
A single private key does not control all the addresses managed by an HD wallet.
* **"(1) sole custody;"**
As above, a single private key has this property for addresses under its control,
but not for all addresses under the control of an HD wallet.
Note also that "sole custody" is a question of how many parties happen to hold a copy of a secret,
not a question of what the secret is.
A 32-byte Ed25519 private key remains a 32-byte Ed25519 private key
whether one party or several have a copy of it.
* **"(2) the ability to both receive and send transactions from a digital wallet;"**
You can say that a private key can "send transactions" in the sense that it can provide
authorization via a digital signature.
But it does not make sense to talk about a private key "receiving transactions",
other than the somewhat convoluted connection to the corresponding public key.
It makes much more sense to talk about a wallet sending and receiving transactions.
* **"(3) the ability to regenerate the private key using a recovery or seed phrase."**
The property of "regenerating the private key" is a property of the seed phrase,
not the private key.
This is the core concept of an HD wallet.

## The redemption code versus the redeem address UTxO

The report says (pp. 40-41), quoting an interviewee:

> The code was intended to be used only once. The moment it is used, it's spent and consumed.

This conflates the *redemption code* with the *redeem address UTxO*.

An Ed25519 private key is not "consumed" by being used to sign a transaction.
It remains valid forever and can produce any number of digital signatures.
What is spent and consumed is the specific UTxO at the redeem address,
and that is true of **every UTxO ever created on Cardano**.
As soon as a transaction spends a UTxO, that UTxO ceases to exist.
This has nothing to do with the redemption code itself,
and nothing to do with the code being "intended to be used only once".

## A final example

The report says (p. 44):

> The Redemption Code only had one output: deliver the ada
from the Genesis Block to the Voucher Holder’s wallet.

I would change this to:

"The Redemption Code is an Ed25519 private key which was used to transfer ADA held in a redeem address UTxO."

## Summary

* The McDermott, Will & Schulte report on the Cardano unredeemed ADA appears to conflate
three distinct concepts: private keys, the seeds used for HD wallets, and the mnemonic
phrases (also called "seed phrases" or "recovery phrases") used to encode those seeds.
* The redemption codes used by the Daedalus wallet to spend redeem address UTxO are Ed25519 private keys.
