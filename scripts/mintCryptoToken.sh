cardano-cli transaction build \
   --babbage-era \
   --cardano-mode \
   --testnet-magic 1 \
   --tx-in-collateral 9c8ec6b8e92867383e153d8d6047c70714be12587b178fc34e9168c3d99c7b90#1 \
   --tx-in 9c8ec6b8e92867383e153d8d6047c70714be12587b178fc34e9168c3d99c7b90#1 \
   --change-address $(cat ~/Cardano/plutus-vasil/plutus-vasil-v2/test-addresses/01.addr) \
   --tx-out "$(cat ~/Cardano/plutus-vasil/plutus-vasil-v2/test-addresses/01.addr) + 3000000 lovelace + 150000 43f46035f41cd8c4d2050d5c1782d4b81b51ecfc42afe1ba87e1e8eb.5a4b526f6c6c7570" \
   --mint "150000 43f46035f41cd8c4d2050d5c1782d4b81b51ecfc42afe1ba87e1e8eb.5a4b526f6c6c7570" \
   --minting-script-file ~/Cardano/plutus-vasil/plutus-vasil-v2/scripts/crypto-token.plutus \
   --mint-redeemer-file ~/Cardano/plutus-vasil/plutus-vasil-v2/test-addresses/unit.json \
   --protocol-params-file ~/Cardano/plutus-vasil/plutus-vasil-v2/test-addresses/protocol.json \
   --out-file tokenMint.unsigned 


