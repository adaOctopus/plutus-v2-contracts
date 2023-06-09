export cryptoToken=$(echo -n "ZKRollup" | xxd -ps | tr -d '\n')
export networkID=1
export addressOne=$(cat test-addresses/01.addr)
export magic="--testnet-magic $networkID"
export queryUTXO="cardano-cli query utxo --address $addressOne $magic"
export queryTip="cardano-cli query tip $magic"
export CARDANO_NODE_SOCKET_PATH=~/Cardano/cardano-testnet/db-preprod/node.socket
export genScriptKeys="cardano-cli address key-gen --verification-key-file test-addresses/cryptoScript.vkey --signing-key-file test-addresses/cryptoScript.skey"
export buildScriptAddress="cardano-cli address build --payment-script-file scripts/crypto-token.plutus $magic --out-file cryptoScript.addr"
export cryptoPolicy="cardano-cli transaction policyid --script-file scripts/crypto-token.plutus"
